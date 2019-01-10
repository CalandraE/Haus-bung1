install.packages("foreach")
install.packages("doParallel")

##### Plan
# get_contrasts_parallel is the paralised version of get_contrasts
# the main change is that the deviation calculation for each row is
# parallised. This parallisation is set up in get_contrasts_parallel while
# the actual calcualtion part is therefore outsorced into get_subspace_deviation
# and called in the paralisation process

####### Function P1: get_subspace_deviation

### Inputs: data,  deviation, slice, draws, seed (as defined in hics-design)
## index_space (index recorded the subpace currently being searched), 
## subspaces (matrix of the column number making each subspace, ...
## from get_subspace_data)

### Discription:
## record the column number of the current subpace in the results_subspace vector
## get the data for the subspace
## record the deviation for that subspace in results_subspace

###  Output: results_subspace 
## (vector containg column number of dim1 and dim2 
## and the devation of the subspace


######## Function P1: get_subspace_deviation
get_subspace_deviation <- function(data, subspaces, deviation, slice, draws, 
                             seed, index_space){
  
  ### empty vector is recorded to hold the results for each row
  results_subspace <- rep(NA, times = 3)
  results_subspace[1] <- subspaces[index_space, 1] 
  results_subspace[2] <- subspaces[index_space, 2]
  
  # get the subset of the data frame needed to the current subspace
  subspace_data <- get_subspace_data(data = data, index_space = index_space,
                                     subspace_combinations = 
                                       subspaces)
  # calculate and record the deviation
  results_subspace[3] <- calculate_contrast(
    subspace_data = subspace_data, slice = slice, deviation = deviation, 
    seed = seed, draws = draws, index_space = index_space)
  
  # return results for the row <index_space>
  results_subspace
}

# my tests: get_subspace_deviation
get_subspace_deviation(grid1, deviation = "tw", seed = 12121,
                       slice = 0.1, subspaces2, draws =100, index_space = 2)
# works as expected
get_subspace_deviation(grid1, deviation = "tw", seed = 12121,
                       slice = 0.1, subspaces2, draws =100, index_space = 5)
# different result for a different row

#####  get_contrasts_parallel 
get_contrasts_parallel <- function(data, spaces = NULL, deviation = c("ks", "cvm", "tw"),
                          slice = 0.2, draws = 1e2, max_spaces = 4950,
                          seed = NULL) {
  library(checkmate)
  library(goftest)
  
  ### INPUT CHECKS
  # must handel the structure of data and spaces
  #### ie rejecting or modifying were nessary
  
  # If data isn't a matrix it must be a data frame object
  if (!is.matrix(data)) {
    assert_data_frame(data)
  }
  
  # if data contains any inapproprate columns these are removed
  data <- modify_data(data)
  
  # if less than two columns are left after modification return error
  if (ncol(data) <= 0) stop("<data> must contain atleast two suitable
                            columns for a devation to be calculated")
  
  # test if all columns in data are (now) numeric
  # as.matrix is nessary to test if the all remainig columns of the dataframe
  # ... are now numeric
  assert_numeric(as.matrix(data))
  
  # spaces isn't null it is tested and (potentially) modified 
  if (!is.null(spaces))   spaces <- modify_spaces(spaces, data)
  
  # Check deviation is a function that returns a numeric value with ... 
  # ... two numeric vectors of different lengths
  deviation_check(deviation)
  
  check_single_number(max_spaces, interger = TRUE)
  check_single_number(draws, interger = TRUE)
  
  # If is a seed is given it must be as single integer
  if (!is.null(seed)) check_single_number(seed)
  # Slice must be a single number between 0 and 1
  check_single_number(slice, limits = c(0,1))
  # slice cannot howeer be 0
  if (slice == 0) stop("Slice cannot be 0")
  
  ###### End of tests
  
  ### Get the matrix with all subspaces that will be explored
  subspaces <- get_subspaces(dimensions = ncol(data), max_spaces = max_spaces
                             ,seed = seed, spaces = spaces)
  
  ### create a results table for the deviation of each each of these subspaces
  ## 3 columns are needed to store the number of the columns comprising ...
  ## ... the subspace as well as the devation calculation
  results <- data.frame(matrix(NA, ncol = 3, nrow = nrow(subspaces)))
  colnames(results) <- c("dim1", "dim2", "deviation")
  
  ########### This step is now being Paralised
  
  ### set up paralliastion
  library(foreach)
  library(doParallel)
  
  ## register paralisation
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl)
  
  ## set up clusters
  # Register all variables and functions used in the parallised step
  clusterExport(cl = cl, 
                varlist = c("data", "spaces", "subspaces", "deviation", "slice" 
                            ,"draws","max_spaces","seed","results",
                            "get_subspace_deviation","check_single_number",
                            "deviation_check","modify_spaces","get_subspaces"
                            # "get_all_subspaces", "get_use_subpaces",
                            ,"get_subspace_data","calculate_contrast",
                            "get_which_conditional","get_data_slice"
                            ,"get_each_deviation")
                ,envir = sys.frame(sys.nframe()))
  
  # fill the results matrix by calculating the devation for indvidual subspaces
  # ... in parallel
  
  foreach(index_space = seq_len(nrow(subspaces))) %dopar% 
  {results[index_space,] <- get_subspace_deviation(data, subspaces, deviation, slice, draws, 
                               seed, index_space)}
  
  # oreder results so that the largest devations are at the top
  results <- results[order(results$deviation, decreasing = TRUE),]
  
  # return the results matrix
  results
}

##### test
get_contrasts_parallel(three_d, deviation = "tw", seed = 12121)
## RETURNS ONLY NAS