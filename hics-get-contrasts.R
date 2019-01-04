# install.packages("goftest")
# install.packages("mvtnorm")
###############################################################################
# Function 1.1: get_all_subspaces

get_all_subspaces <- function(dimensions){
  # Convert dimensions (number of columns) into a vector
  dimensions_vector <- c(1:dimensions)
  
  # "combn" with 2 gets all the unique combinations of 2 entries in dimensions_vector
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
  
  ## get a vector with the the subspace combination row numbers
  all_subspaces_rows <- c(1:nrow(all_subspaces))
  
  ## set seed if the is one as the one given to the function
  if (!is.null(seed)) set.seed(seed)
  
  ## draws row number with replacing them
  choosen_subspaces_rows <- sample(all_subspaces_rows,
                                   size = n_o_spaces,
                                   replace = FALSE)
  
  ## Return those rows of the subspace combinations choosen in the sample
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
  
  #### If no value for <spaces> was entered into the function then use ...
  ## ... then get_all_subspaces to get all possible subspaces
  #### It is possible that the <spaces> entered into the function is ...
  ## ... bigger than allowed by <max_spaces> this implimentation alows ...
  ## a random selection of a subset of thoses user given spaces for testing
  
  ## If <spaces> are given by the user these form the subspaces from which ...
  ## ... are randomly selected
  all_subspaces <- spaces
  
  ## if no entry for <spaces> was given the get_all_subspaces function is used
  if (is.null(all_subspaces)) all_subspaces <- get_all_subspaces(dimensions)
  
  #### If <max_spaces> is larger than or equal to the total number of ...
  # ... subspaces then all combinations are returned
  if (max_spaces >= nrow(all_subspaces)) return(all_subspaces)
  
  ### otherwise max_space determins the number of combinations explored
  # "number of" is abrevated to n_o 
  n_o_spaces <- max_spaces
  
  ### Otherwise a subset of combinations choosen randomly are returned
  get_use_subpaces(all_subspaces, n_o_spaces, seed)
}

## My tests for get_subspaces
subspaces1 <- get_subspaces(dimensions = 10, max_spaces = 10, seed = 1, spaces = NULL)
subspaces1 # As expected gets 10 randomly choosen combinations

subspaces2 <- get_subspaces(dimensions = 10, max_spaces = 100, seed =  1, spaces = NULL)
identical(subspaces2, get_all_subspaces(dimensions = 10))
# As expected if max_spaces exceeds the number of combinations ...
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
# returns a slice of the data ordered by the the second column with ...
# enough rows to contain 0.05 of the rows in the inputed dataset


############################
##### Function 3.2: get_each_deviation
get_each_deviation <- function(independant_marginal, independant_conditional,
                               deviation) {
  # To allow for the posibility that mulitple methods are given for <devation> ...
  # ... each method is applied and the results averaged
  
  # create vector to store the results in results
  devation_results <- rep(NA, times = length(deviation))
  
  # apply each devation method given to independant_marginal and ...
  # independant_conditional filling in the results in the devation_results vector
  
  # For improved readability the function inputs are abrevated
  ic <- as.matrix(independant_conditional)
  im <- as.matrix(independant_marginal)
  
  for (k in 1:length(deviation)) {
    # If devation[k] is one of the expected functions then  ...
    # ... 1 - their p value is reccored. If a custom function is given
    # then that function is applied
    
    if (!(deviation[k] %in%  c("ks", "cvm", "tw"))) {
      devation_results[k] <- apply(ic, im, MARGIN = 2, FUN = deviation[k])      
    } else       devation_results[k] <-
        switch(deviation[k] 
               ,"ks" = 1 - suppressWarnings(stats::ks.test(ic,im, "two.sided"))$p.value
               ,"cvm" = 1 - goftest::cvm.test(ic, null = ecdf(im))$p.value
               ,"tw" = 1 - stats::t.test(ic,im, "two.sided")$p.value)
    
    # Note to the case "cvm": "ecdf(im)" gets the marginal empirical ...
    # cumulative distribution function of the independant variable.
    # "goftest::cvm.test" then tests if ic follows the same distrubution
    
    # Note to the case "ks": suppressWarnings is used to supress the ...
    # ... message "p-value will be approximate in the presence of ties".
    # This message is deemed unnessary since the other methods like ...
    # ... "cvm" will also give approximate results (because it approximates ...
    # ... the marginal distrbution). Besides the results of mulitple methods ...
    # ... are averaged anyway.
    
  }
  
  # return a mean over the deviation method results
  mean(devation_results)
}

# my tests: get_each_deviation
### The inputs are the independant variables (ie column that was not conditioned on)
### from the whole dataset (marginal) and from the slice (conditional)
independant_marginal <- subspace_data2[,1] 
independant_conditional <- slice2[,1] # collumn that was not conditioned on

dev1 <- get_each_deviation(independant_marginal, 
                           independant_conditional, deviation = "ks")
dev1
dev2 <- get_each_deviation(independant_marginal, 
                           independant_conditional, deviation = "cvm")
dev2 # very differnt results two other two methods
dev3 <- get_each_deviation(independant_marginal, 
                           independant_conditional, deviation = "tw")
dev3

dev4 <- get_each_deviation(independant_marginal, 
                           independant_conditional, 
                           deviation = c("ks","cvm","tw"))
all.equal(dev4,mean(c(dev1,dev2,dev3))) 
# as expected the funktion returns the mean result of all methods given ...
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
    
    # vector for estimating the marginal distrebution of the independant variable
    #### Note: subspace_data has two collumns. The one not conditioned on is ...
    #### ... therefore the independant variable
    independant_marginal <- subspace_data[,-conditional_index]
    
    # vector for estimating the distrubtion of the same variable conditioned ...
    # ... on the other
    independant_conditional <- get_data_slice(subspace_data = subspace_data,
                                              conditional_index = conditional_index,
                                              seed = seed, slice = slice,
                                              index_draw = index_draw,
                                              index_space = index_space)
    
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

###############################################################################
##### Function 0: get_contrasts 
get_contrasts <- function(data, spaces = NULL, deviation = c("ks", "cvm", "tw"),
                          slice = 0.2, draws = 1e2, max_spaces = 4950,
                          seed = NULL) {
  library(checkmate)
  library(goftest)
  # TODO
  ### INSERT THOUGHER INPUT CHECKS
  
  ### Get the matrix with all subspaces that will be explored
  subspaces <- get_subspaces(dimensions = ncol(data), max_spaces = max_spaces
                             , seed = seed, spaces = spaces)
  
  ### create a results table for the deviation of each each of these subspaces
  ## 3 columns are needed to store the number of the columns comprising ...
  ## ... the subspace as well as the devation calculation
  results <- data.frame(matrix(NA, ncol = 3, nrow = nrow(subspaces)))
  colnames(results) <- c("dim1", "dim2", "deviation")
  
  ### for each subspace the devation is calculated and recorded in the ...
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
  # oreder results so that the largest devations are at the top
  results <- results[order(results$deviation, decreasing = TRUE),]
  
  # return the results matrix
  results
}

###############################################################################
################################ Official Tests ###############################

set.seed(1212)
source("hics-get-contrasts.R")
#simple 2D datasets:
grid <- as.matrix(expand.grid(x = 1:10, y = 1:10)) + .1 * matrix(rnorm(200), 100, 2)
grid <- grid[sample(100),]
line <- cbind(x = 1:100, y = 1:100)
fuzzy_line <- line + 10 * matrix(rnorm(200), 100, 2)
examples <- cbind(line, fuzzy_line, grid)
#-------------------------------------------------------------------------------
# CHECK: yield sensible results for defaults:
isTRUE(get_contrasts(grid)$deviation < .2) # doesn't work yet
isTRUE(get_contrasts(line)$deviation > .9) # is true
isTRUE(get_contrasts(fuzzy_line)$deviation <
         get_contrasts(line)$deviation) # is true
contrasts_examples <- get_contrasts(examples)
# all subspaces are covered:
nrow(contrasts_examples) == choose(6, 2) # is true
# 1st column has smaller column numbers, 2nd the higher ones:
all(sort(unique(contrasts_examples[, 1])) == 1:(ncol(examples) - 1)) # is true
all(sort(unique(contrasts_examples[, 2])) == 2:ncol(examples)) # is true
# rows are sorted high to low according to deviation:
all(rank(contrasts_examples$deviation) == 15:1) # is true
#"line" has highest contrast, "grid" has lowest contrast
all(contrasts_examples[1, 1:2] == c(1, 2)) # doesn't work yet
all(contrasts_examples[15, 1:2] == c(5, 6)) # is true
# default deviation is 1 - p-value:
all(contrasts_examples$deviation > 0 & contrasts_examples$deviation < 1) # is true
#-------------------------------------------------------------------------------
# CHECK: yields reproducible results:
identical(get_contrasts(examples, seed = 12121),
          get_contrasts(examples, seed = 12121)) # is TRUE
#-------------------------------------------------------------------------------
# CHECK: cleans up inputs:

get_contrasts(cbind(grid, NA, 1), seed = 12121) # should trigger warning(s)
## DOESN'T WORK YET
identical(get_contrasts(cbind(grid, NA, 1), seed = 12121),
          get_contrasts(grid, seed = 12121)) ## DOESN'T WORK YET
#-------------------------------------------------------------------------------
# CHECK: accepts (and cleans up) data.frames:
df_examples <- cbind(as.data.frame(examples),
                     some_factor = gl(10, 10))
# this should give a warning:
get_contrasts(df_examples, seed = 12121) ## DOESN'T WORK YET
# irrelevant columns are ignored/removed:
identical(get_contrasts(df_examples, seed = 12121),
          get_contrasts(examples, seed = 12121)) ## DOESN'T WORK YET
#-------------------------------------------------------------------------------
# CHECK: implements different types of deviation measures correctly:
set.seed(122)
# three_d has strong correlation for (1,2), medium for (1,3), zero for (2, 3)
sigma <- matrix(c(1,.9, .4, .9, 1, 0, .4, 0, 1), 3, 3)

three_d <- mvtnorm::rmvnorm(500, sigma = sigma)
# same order of subspaces for different deviations:
!identical(get_contrasts(three_d, deviation = "cvm", seed = 12121),
           get_contrasts(three_d, deviation = "ks", seed = 12121)) # is TRUE
identical(get_contrasts(three_d, deviation = "cvm", seed = 12121)[,1:2],
          get_contrasts(three_d, deviation = "ks", seed = 12121)[,1:2]) # doesn't work yet
!identical(get_contrasts(three_d, deviation = "cvm", seed = 12121),
           get_contrasts(three_d, deviation = "tw", seed = 12121)) # is TRUE
identical(get_contrasts(three_d, deviation = "cvm", seed = 12121)[,1:2],
          get_contrasts(three_d, deviation = "tw", seed = 12121)[,1:2]) # doen't work yet
# accepts user defined functions for deviation:
ks_user <- function(conditional, marginal) {
  1 - ks.test(conditional, marginal)$p.value
}
identical(get_contrasts(three_d, deviation = ks_user, seed = 12121),
          get_contrasts(three_d, seed = 12121)) # Doesn't work yet
#-------------------------------------------------------------------------------
# CHECK: implements max_spaces arg correctly:
get_contrasts(examples, max_spaces = 2)
#above should give an informative warning that not all sub-spaces are explored.
isTRUE(nrow(get_contrasts(examples, max_spaces = 7)) == 7L)
#reproducible results:
identical(get_contrasts(examples, seed = 12121, max_spaces = 3),
          get_contrasts(examples, seed = 12121, max_spaces = 3)) # is TRUE
highdim <- matrix(runif(1e4), ncol = 1e3, nrow = 10)
isTRUE(nrow(get_contrasts(highdim, draws = 1)) == choose(100, 2)) # is TRUE
#-------------------------------------------------------------------------------
# CHECK: implements spaces arg correctly:
identical(get_contrasts(three_d, spaces = combn(1:3, 2), seed = 12121),
          get_contrasts(three_d, seed = 12121)) # doesn't Work yet
identical(get_contrasts(three_d, spaces = rbind(2, 3))[1, -3],
          c(2, 3))  # doesn't Work yet
get_contrasts(three_d, spaces = cbind(c(3, 2), c(2, 3), c(2, 3))) # doen't work yet
# above should give an informative warning...
# ... and remove redundant spaces:
identical(get_contrasts(three_d, spaces = rbind(2, 3), seed = 12121),
          get_contrasts(three_d, spaces = cbind(c(3, 2), c(2, 3), c(2, 3)),
                        seed = 12121)) # doesn't work yet
#-------------------------------------------------------------------------------
# FAILS: the calls below should all fail with INFORMATIVE, precise error messages
get_contrasts(as.character(grid))
get_contrasts(matrix(1, 10, 10))
get_contrasts(data.frame(gl(10, 10), gl(5, 20)))
get_contrasts(line, slice = "a")
get_contrasts(line, slice = 1*NA)
get_contrasts(line, slice = 0)
get_contrasts(line, slice = 2)
get_contrasts(line, slice = c(1, 2))
get_contrasts(line, draws = "a")
get_contrasts(line, draws = 1*NA)
get_contrasts(line, draws = 0.5)
get_contrasts(line, draws = c(1, 2))
get_contrasts(line, deviation = "sk")
bs <- function(x1, x2) {
  cat("bs!")
}
get_contrasts(line, deviation = bs)
get_contrasts(examples, max_spaces = "a")
get_contrasts(examples, max_spaces = NA)
get_contrasts(examples, max_spaces = -1)
get_contrasts(examples, spaces = expand.grid(1:2, 1:3))
get_contrasts(examples, spaces = combn(1:7, 2))

