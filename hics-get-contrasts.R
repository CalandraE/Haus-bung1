# install.packages("goftest")
# install.packages("mvtnorm")
###############################################################################
# Function 1.1: get_all_subspaces

get_all_subspaces <- function(dimensions){
  # Convert dimensions (number of columns) into a vector
  dimensions_vector <- c(1:dimensions)
  
  # "combn" with “2” gets all the unique combinations of 2 entries in dimensions vector
  # It needs to be transposed to get the numbers in the columns
  all_combinations <- t(combn(dimensions_vector, 2)) 
  
  all_combinations
}

## my tests for get_all_subspaces
dim1 <- 3
all_subspaces1 <- get_all_subspaces(dim1)
all_subspaces1

dim2 <- 10
all_subspaces2 <- get_all_subspaces(dim2)
all_subspaces2
#############################
# Function 1.2: get_use_subpaces

get_use_subpaces <- function(all_subspaces, n_o_spaces, seed){
  ## This function is only called if  <max_spaces> are smaller ... 
  ## ... than the maximum number of subspaces
  ## In this function <max_spaces> subspaces are randomly selected
  
  ## get a vector with the subspace combination row numbers
  all_subspaces_rows <- c(1:nrow(all_subspaces))
  
  ## set seed if the is one as the one given to the function
  if (!is.null(seed)) set.seed(seed)
  
  ## draws row number with replacing them
  choosen_subspaces_rows <- sample(all_subspaces_rows,
                                   size = n_o_spaces,
                                   replace = FALSE)
  
  ## Return those rows of the subspace combinations chosen in the sample
  all_subspaces[choosen_subspaces_rows,]
}

## my tests for get_use_subspaces

use_subspace1.2.1 <- get_use_subpaces(all_subspaces1, n_o_spaces = 2, seed = 1)
use_subspace1.2.1 # returns as expected only 2 combinations
use_subspace1.2.2 <- get_use_subpaces(all_subspaces1, n_o_spaces = 2, seed = 120)
use_subspace1.2.2
identical(use_subspace1.2.1, use_subspace1.2.2) 
# As expected with a different seed a different subset of combinations are selected

############################
##### Function 1: get_subspaces 

get_subspaces <- function(dimensions, max_spaces, seed, spaces){
  
  #### If no value for <spaces> was given to the function then use ...
  ## ... then get_all_subspaces to get all possible subspaces
  #### It is possible that the <spaces> given to the function is ...
  ## ... bigger than allowed by <max_spaces> this implementation allows ...
  ## a random selection of a subset of those user given spaces
  
  ## If <spaces> are given by the user these form the subspaces from which ...
  ## ... are randomly selected
  all_subspaces <- spaces
  
  ## if no entry for <spaces> was given the get_all_subspaces function is used
  if (is.null(all_subspaces)) all_subspaces <- get_all_subspaces(dimensions)
  
  #### If <max_spaces> is larger than or equal to the total number of ...
  # ... subspaces then all combinations are returned. Otherwise give a warning
  if (max_spaces >= nrow(all_subspaces)) return(all_subspaces)
  else warning("Not all possible subspaces have been explored")
  
  ### otherwise max_space determines the number of combinations explored
  # "number of" is abrevated to n_o 
  n_o_spaces <- max_spaces
  
  ### Otherwise a subset of combinations chosen randomly are returned
  get_use_subpaces(all_subspaces, n_o_spaces, seed)
}

## My tests for get_subspaces
subspaces1 <- get_subspaces(dimensions = 10, max_spaces = 10, seed = 1, spaces = NULL)
subspaces1 # As expected gets 10 randomly chosen combinations

subspaces2 <- get_subspaces(dimensions = 10, max_spaces = 100, seed =  1, spaces = NULL)
identical(subspaces2, get_all_subspaces(dimensions = 10))
# As expected, if max_spaces exceeds the number of combinations ...
# ... all combinations are returned

###############################################################################
##### Function 2: get_subspace_data
get_subspace_data <- function(data, index_space, subspace_combinations){
  
  # get the column numbers of the relevant combination out of subspace_combinations
  dim1 <- subspace_combinations[index_space,1]
  dim2 <- subspace_combinations[index_space,2] 
  
  ### return the subset of the data containing only these two columns
  data[,c(dim1,dim2)]
}


## my tests: get_subspace_data

grid1 <- matrix(rnorm(5),nrow = 20, ncol = 10)   

#### should still be defined above
# subspaces1 <- get_subspaces(dimensions = 10, max_spaces = 10, seed = 1)
subspace_data1 <- get_subspace_data(data = grid1, index_space = 1, 
                                    subspace_combinations = subspaces1)
subspace_data1 ## As expected it returns 2 columns from the dataset

subspaces1[1,] # the first row in subspaces1 refers to columns 2 and 5
identical(subspace_data1, grid1[,subspaces1[1,]], grid1[,c(2,5)])
## As expected all approaches access the same two rows of data

###############################################################################
##### Function 3.1: get_which_conditional

get_which_conditional <- function(seed = NULL, index_space, index_draw){
  
  # If a seed is given as a user then the seed is set for the random draw
  if (!is.null(seed)) set.seed((seed*index_space) + index_draw)
  
  # Return a randomly return either 1 or 2
  sample(c(1,2), size = 1)
}

## my tests: get_which_conditional
get_which_conditional(seed = 123, index_draw = 1, index_space = 10) # returns 1
get_which_conditional(seed = 1234, index_draw = 1, index_space = 10) # returns 2
get_which_conditional(index_draw = 1, index_space = 10) # works without a seed

############################
##### Function 3.2: get_data_slice
get_data_slice <- function(subspace_data, conditional_index, slice, 
                           seed = NULL, index_space, index_draw){
  #### Order the matrix subspace_data by the conditional variable
  ## The conditional variable is accessed by finding the column in ...
  ## ... subspace_data that was specified by conditional_index (1 or 2)
  ordered_data <- subspace_data[order(subspace_data[,conditional_index]),]
  
  # get width of the slice which contains at least <slice> of the data rows
  slice_width <- ceiling(slice*nrow(ordered_data))
  
  # Before a random selection is made a seed can be set
  if (!is.null(seed)) set.seed((seed*index_space) + index_draw)
  
  # Randomly choose the starting point for the slice making sure there ...
  # ... are still atleast <slice_width> further rows below it
  starting_point <- sample(c(1:(nrow(ordered_data) - slice_width)), size = 1)
  
  # return <slice_width> rows of the ordered dataset starting at <starting_point>
  # "- 1" because the slice_width includes the starting point row
  ordered_data[c(starting_point:(starting_point + slice_width - 1)),]
}

# my tests: get_data_slice
grid2 <- matrix(rnorm(200*10),nrow = 200, ncol = 10) 
subspace_data2 <- get_subspace_data(grid2, index_space = 1, 
                                    subspace_combinations = subspaces1)
slice2 <- get_data_slice(subspace_data2, conditional_index = 2, slice = 0.05, 
                         seed = 1234, index_space = 3, index_draw = 1) 
slice2 
# returns a slice of the data ordered by the second column with ...
# enough rows to contain 0.05 of the rows in the inputted dataset


############################
##### Function 3.3: get_each_deviation
get_each_deviation <- function(independant_marginal, independant_conditional,
                               deviation) {
  # To allow for the possibility that multiple methods are given for <devation> ...
  # ... a vector is allowed as an input but only the first entry is used
  
  
  # apply each deviation method given to independant_marginal and ...
  # independant_conditional filling in the results in the devation_results vector
  
  # For improved readability the function inputs are abbreviated
  ic <- as.matrix(independant_conditional)
  im <- as.matrix(independant_marginal)
  
  # Case 1: deviation is a character or character vector
  if (is.character(deviation)) {
    
    # If deviation[1] is one of the expected functions then  ...
    # ... 1 - their p value is returned. If a custom function is given
    # then that function is applied
    
    if (!(deviation[1] %in%  c("ks", "cvm", "tw"))) {
      return(
        suppressWarnings(apply(ic, im, MARGIN = 2, FUN = deviation[1])))
    } else 
      return(switch(deviation[1] 
                    ,"ks" = 
                      1 - suppressWarnings(stats::ks.test(ic, im, exact = TRUE))$p.value
                    ,"cvm" = 1 - goftest::cvm.test(ic, null = ecdf(im))$p.value
                    ,"tw" = 1 - stats::t.test(ic, im, exact = TRUE)$p.value))
    
    # Note to the case "cvm": "ecdf(im)" gets the marginal empirical ...
    # cumulative distribution function of the independent variable.
    # "goftest::cvm.test" then tests if “ic” follows the same distribution
    
    # Note to the case "ks": suppressWarnings is used to supress the ...
    # ... message "p-value will be approximate in the presence of ties".
    # This message is deemed unnecessary since the other methods like ...
    # ... "cvm" will also give approximate results (because it approximates ...
    # ... the marginal distribution). Besides the results of multiple methods ...
    # ... are averaged anyway.
    
  }
  
  # Case 2: deviation is a test Function
  
  # return value of the function - suppressing warnings that might ensue
  suppressWarnings(deviation(ic,im))
  
}

# my tests: get_each_deviation
### The inputs are the independent variables (i.e. column that was not conditioned on)
### from the whole dataset (marginal) and from the slice (conditional)
independant_marginal <- subspace_data2[,1] 
independant_conditional <- slice2[,1] # column that was not conditioned on

dev1 <- get_each_deviation(independant_marginal, 
                           independant_conditional, deviation = "ks")
dev1
dev2 <- get_each_deviation(independant_marginal, 
                           independant_conditional, deviation = "cvm")
dev2 # very different results two other two methods
dev3 <- get_each_deviation(independant_marginal, 
                           independant_conditional, deviation = "tw")
dev3

ks_user <- function(conditional, marginal) {
  1 - ks.test(conditional, marginal)$p.value
}
dev4 <- get_each_deviation(independant_marginal, 
                           independant_conditional, deviation = "ks_user")
dev4

dev5 <- get_each_deviation(independant_marginal, 
                           independant_conditional, deviation = ks_user)
dev5
# dev4 <- get_each_deviation(independant_marginal, 
#                            independant_conditional, 
#                            deviation = c("ks","cvm","tw"))
#all.equal(dev4,mean(c(dev1,dev2,dev3))) 
# as expected the function returns the mean result of all methods given ...
# in <deviation>

############################
##### Function 3: calculate_contrast
calculate_contrast <- function(subspace_data, slice, deviation, seed = NULL,
                               draws, index_space){
  # vector for storing the results of each draw run though
  draws_results <- rep(NA, times = draws)
  
  for (index_draw in seq_along(1:draws)) {
    # randomly select a column to be conditioned on
    conditional_index <- get_which_conditional(seed = seed, 
                                               index_space = index_space,
                                               index_draw = index_draw)
    
    # vector for estimating the marginal distribution of the independent variable
    #### Note: subspace_data has two columns. The one not conditioned on is ...
    #### ... therefore the independent variable
    independant_marginal <- subspace_data[,-conditional_index]
    
    # vector for estimating the distribution of the same variable conditioned ...
    # ... on the other
    independant_conditional <- get_data_slice(subspace_data = subspace_data,
                                              conditional_index = 
                                                conditional_index,
                                              seed = seed, slice = slice,
                                              index_draw = index_draw,
                                              index_space = index_space)[
                                                ,-conditional_index]
    
    # fill the results index with the calculated deviation
    draws_results[index_draw] <- get_each_deviation(
      independant_marginal = independant_marginal, 
      independant_conditional = independant_conditional, deviation = deviation)
  }
  # return the mean result of all draws
  mean(draws_results)
}

# my tests: calculate_contrast
constrast1 <- calculate_contrast(subspace_data2, slice = 0.05, seed = 1234, 
                                 index_space = 3, draws = 20, 
                                 deviation = c("ks","cvm","tw"))
constrast1 # As expected a value between 0 and 1 is returned

constrast2 <- calculate_contrast(subspace_data2, slice = 0.05, seed = 12345, 
                                 index_space = 3, draws = 20, 
                                 deviation = c("ks","cvm","tw"))
constrast2 
# As expected for a slightly different seed a slightly different result is given

constrast3 <- calculate_contrast(subspace_data2, slice = 0.05, seed = 1234, 
                                 index_space = 3, draws = 20, 
                                 deviation = "ks") 
identical(constrast1,constrast3) 
# as expected only the first method in <devation> is used 

###############################################################################

##### Function 0.1: check_single_number
check_single_number <- function(argument, interger = FALSE, limits = NULL){
  if (interger == TRUE) checkmate::assert_int(argument)
  
  # assert_number checks if a single numeric value is given
  checkmate::assert_number(argument)
  
  # given an error message if argument isn't within the limits 
  # (if there are any)
  if (!is.null(limits)) {
    if (!((argument >= limits[1]) && (argument <= limits[2]))) stop(
      paste(as.character(bquote(argument))," must be in [", limits[1], ","
            ,limits[2],"]", sep = "")
    )
  }
}
### My test: check_single_number
check_single_number("text") # returns suitable error message
check_single_number(c(1,2)) # returns suitable error message
check_single_number(c(1.2), interger = TRUE) # returns suitable error message
too_big <- 1.2
check_single_number(too_big, interger = FALSE, limits = c(0,1))
# returns the expected error message in the correct form

##### Function 0.2: deviation_check
deviation_check <- function(deviation){
  
  # If deviation is given a function name it is processed differently ...
  # ... to cases where a function itself is given. 
  
  # if deviaiton is neither a function nor a character (vector) give an error
  if (!(is.function(deviation) | is.character(deviation))) {
    stop("deviation must be a function or the name of a function")
  }
  
  # Create test inputs (used in both cases)
  test_marginal <- as.matrix(rnorm(100, mean = 0, sd = 1))
  test_conditional <-  as.matrix(rnorm(10, mean = 0, sd = 1))
  
  ### Case 1: Function name is given a character (or character vector)
  
  # If a vector is given the only the first method is used
  if (!is.function(deviation)) {
    method <- deviation[1]
    
    # Check that <deviation> is given as a character vector
    checkmate::assert_character(method)
    
    # if the deviation function is one of the defaults no further checks are needed
    if (method %in% c("ks","cvm","tw"))  return() # return nothing
    
    # test if a function with this name exists
    ### if the function does exist then a character vector is returned ...
    ### ... containing the names of Environments containing a function of ...
    ### ... that name. If it has length 0 then the 'function' can't be found ...
    ### …. anywhere
    
    if (length(find(method, mode = "function")) == 0 ) {
      stop(paste("No function called", as.character(bquote(method)), "exists
                 in any environments currently loaded", sep = " "))
    } 
    
    # check that the method returns a single numeric value between 0 and 1
    check_single_number(apply(test_conditional, test_marginal, 
                              MARGIN = 2, FUN = method))   
    
    } else {
      # Case 2: a function is given
      method <- deviation # method is now simply the deviation function given
      
      # check if the method returns a single numeric value between 0 and 1
      check_single_number(method(test_conditional,test_marginal))
    }
  # return nothing: if there are problems the checks would end the function
}
#### my test:
ks_user <- function(conditional, marginal) {
  1 - ks.test(conditional, marginal)$p.value
}
check_deviation1 <- deviation_check("ks_user") 
# no error message as expected

check_deviation2 <- deviation_check(ks_user) 
# no error message as expected

fail <- function(conditional, marginal){
  useless1 <- conditional
  useless2 <- marginal
  "you suck!"
}
check_deviation_fail1 <- deviation_check("fail")
# As expected the "fail" function isn't accepted as a deviation method

check_deviation_fail1 <- deviation_check(fail)
# As expected the "fail" function isn't accepted as a deviation method

#### Function 0.3: modify_data
modify_data <- function(data){
  # columns of the data frame that are not numeric should be ignored ...
  # ... and therefore removed from the data frame for further use
  # However a warning should be given
  
  # create an empty vector store the column numbers of non-numeric columns
  column_remove <- rep(NA, times = ncol(data))
  for (i in seq_along(1:ncol(data))) {
    if (!is.numeric(data[,i])) {
      column_remove[i] <- i
      warning(paste("Column",i, "in <data> isn't numeric and has been removed",
                    sep = " "))
    }
    # if all value in a column are the same it is removed, and a warning is given
    if (!(length(unique(data[,i])) > 1)) {
      column_remove[i] <- i
      warning(paste("All entries in column", i, "in <data> are exactly the 
                    same, this column was therefore removed", sep = " "))
    }  
    }
  # return the dataset with the column_remove columns (if any) removed
  data[,which(!(c(1:ncol(data)) %in% column_remove))]
  }

#### my Test: modify_data
grid <- as.matrix(expand.grid(x = 1:10, y = 1:10)) + .1 * matrix(rnorm(200), 100, 2)
grid <- grid[sample(100),]
line <- cbind(x = 1:100, y = 1:100)
fuzzy_line <- line + 10 * matrix(rnorm(200), 100, 2)
examples <- cbind(line, fuzzy_line, grid)

df_examples <- cbind(as.data.frame(examples),
                     some_factor = gl(10, 10))
modify_data(df_examples) # removes factors

df_examples2 <- cbind(as.data.frame(examples),
                      1,2)
modify_data(df_examples2) # removes repeate columns

df_examples3 <- cbind(as.data.frame(examples),
                      c("a","b"))
modify_data(df_examples3) # removes character vectors

#### Function 0.4: modify_spaces
modify_spaces <- function(spaces, data){
  # check spaces is a numeric matrix
  assert_numeric(spaces)
  assert_matrix(spaces)
  
  # special case: spaces has 2 rows but not 2 columns ...
  # ... in this case the matrix is transposed
  if ((nrow(spaces) == 2)  & (ncol(spaces) != 2 )) { 
    spaces <- t(spaces)
    warning("The spaces matrix has been transposed")
  }
  
  # remove extra columns if there are more than 2 and give a warning
  if (ncol(spaces) > 2) {
    spaces <- spaces[,c(1,2)]
    warning(paste("Only the first 2 columns of spaces are used. Column(s):", c(3:ncol(spaces)), 
                  "have been removed", sep = " "))
  }
  
  # Note in some cases earlier transformations can stop spaces from being a ...
  # ... matrix, this is reversed here. As matrix Automatically turns vectors ...
  # ... into n x 1 matrices. It is therefore transposed to get two columns
  if (!is.matrix(spaces)) spaces <- t(as.matrix(spaces))
  
  # remove rows containing values that don't correspond to columns in data
  permitted_numbers <- c(1:ncol(data)) # to identify a column in data ...
  # ... the values must be integers between 1 and the total number of columns
  
  # vector to record rows being removed
  row_remove <- rep(NA, times = nrow(spaces)) 
  
  
  # test each row
  for (i in seq_along(1:nrow(spaces))) {
    # if either value isn't in the permitted numbers then the row number is ...
    # ... recorded
    if (!all(spaces[i,] %in% permitted_numbers)) {
      stop(paste("Some of the values in row", i, 
                 "in spaces doesn't correspond to a columns in data.",
                 "Therefore, isn't doesn't correspond to a 
                 valid subspaces."
                 , sep = " "))
    }
    if (identical(spaces[i,1], spaces[i,2])) {
      row_remove[i] <- i
      warning(paste("The values in row", i, "in spaces are identical. 
                    This row was removed.", sep = " "))
    }
    
    # remove redundant rows giving a warning
    if (i > 1) { # don't use if the loop is on the first row
      for (j in seq_along(1:(i - 1))) { # apply to all presiding rows
        # check to see if the two rows contain the same values (any order)
        if (all(c(spaces[i,1], spaces[i,2]) %in% c(spaces[j,1], spaces[j,2]))) {
          row_remove[i] <- i
          warning(paste("The values in row", i, "are the same as row", j,
                        "there for row i was removed.", sep = " "))
        }
      }
    }
    
    }
  
  # remove rows that need removing
  spaces <- spaces[which(!(c(1:nrow(spaces)) %in% row_remove)),]
  
  # if spaces is now a vector it converted to a matrix ...
  # ... (transposed two get 2 columns)
  if (!is.matrix(spaces)) spaces <- t(as.matrix(spaces))
  
  # to make order of the values in the rows irrelevant they are sorted ...
  # ... by row
  spaces <- t(apply(spaces,1,sort))
  
  # the now potentially transformed spaces vector is returned
  spaces
}

### my test: modify_spaces
spaces_needs_transposing <- cbind(c(3, 2), c(2, 3), c(2, 3))
sigma <- matrix(c(1,.9, .4, .9, 1, 0, .4, 0, 1), 3, 3)
set.seed(112)
data1 <- mvtnorm::rmvnorm(100, sigma = sigma)
modify_spaces(spaces_needs_transposing, data1) 
# correct shape and correct warning

spaces_1column <- rbind(c(1,2))
modify_spaces(spaces_1column, data1) # correct shape, warning missing

spaces_extra_columns <- cbind(c(1,2,3),c(2,3,2),c(3,1,1))
modify_spaces(spaces_extra_columns, data1) # correct shape and correct warning

spaces_invalid_subspace <- cbind(c(1,2), c(3,2))
modify_spaces(spaces_invalid_subspace, data1) 
# correct shape and correct warning

spaces_invalid_row <- cbind(c(1,2), c(3,4))
modify_spaces(spaces_invalid_row, data1) 
# returns an error

spaces_redundant <- rbind(c(1,2),c(2,3),c(1,2),c(2,3))
modify_spaces(spaces_redundant, data1) 
# correct shape and correct warning

######################
##### Function 0: get_contrasts 
get_contrasts <- function(data, spaces = NULL, deviation = c("ks", "cvm", "tw"),
                          slice = 0.2, draws = 1e2, max_spaces = 4950,
                          seed = NULL) {
  library(checkmate)
  library(goftest)
  
  ### INPUT CHECKS
  # must handle the structure of data and spaces
  #### i.e. rejecting or modifying were necessary
  
  # If data isn't a matrix it must be a data frame object
  if (!is.matrix(data)) {
    assert_data_frame(data)
  }
  
  # if data contains any inappropriate columns these are removed
  data <- modify_data(data)
  
  # if less than two columns are left after modification return error
  if (ncol(data) <= 0) stop("<data> must contain atleast two suitable
                            columns for a devation to be calculated")
  
  # test if all columns in data are (now) numeric
  # as.matrix is necessary to test if the all remaining columns of the data frame
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
  # slice cannot however be 0
  if (slice == 0) stop("Slice cannot be 0")
  
  ###### End of tests
  
  ### Get the matrix with all subspaces that will be explored
  subspaces <- get_subspaces(dimensions = ncol(data), max_spaces = max_spaces
                             , seed = seed, spaces = spaces)
  
  ### create a results table for the deviation of each of these subspaces
  ## 3 columns are needed to store the number of the columns comprising ...
  ## ... the subspace as well as the deviation calculation
  results <- data.frame(matrix(NA, ncol = 3, nrow = nrow(subspaces)))
  colnames(results) <- c("dim1", "dim2", "deviation")
  
  ### for each subspace the deviation is calculated and recorded in the ...
  ### ... Results matrix
  for (index_space in seq_along(1:nrow(subspaces))) {
    # recorded the columns number used in the current subspace
    results[index_space, "dim1"] <- subspaces[index_space, 1]
    results[index_space, "dim2"] <- subspaces[index_space, 2]
    
    # get the subset of the data frame needed to the current subspace
    subspace_data <- get_subspace_data(data = data, index_space = index_space,
                                       subspace_combinations = 
                                         subspaces)
    # calculate and record the deviation
    results[index_space, "deviation"] <- calculate_contrast(
      subspace_data = subspace_data, slice = slice, deviation = deviation, 
      seed = seed, draws = draws, index_space = index_space)
  }
  # order results so that the largest deviations are at the top
  results <- results[order(results$deviation, decreasing = TRUE),]
  
  # return the results matrix
  results
}

