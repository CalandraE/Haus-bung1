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

get_use_subpaces <- function(all_subspaces, spaces, seed){
  ## This function is only called if  <max_spaces> are smaller ... 
  ## ... than the maximum number of subspaces
  ## In this function <max_spaces> subspaces are randomly selected
  
  ## get a vector with the the subspace combination row numbers
  all_subspaces_rows <- c(1:nrow(all_subspaces))
  
  ## set seed if the is one as the one given to the function
  if (!is.null(seed)) set.seed(seed)
  
  ## draws row number with replacing them
  choosen_subspaces_rows <- sample(all_subspaces_rows,
                                   size = spaces,
                                   replace = FALSE)
  
  ## Return those rows of the subspace combinations choosen in the sample
  all_subspaces[choosen_subspaces_rows,]
}

## my tests for get_use_subspaces

use_subspace1.2.1 <- get_use_subpaces(all_subspaces1, spaces = 2, seed = 1)
use_subspace1.2.1 # returns as expected only 2 combinations
use_subspace1.2.2 <- get_use_subpaces(all_subspaces1, spaces = 2, seed = 120)
use_subspace1.2.2
identical(use_subspace1.2.1, use_subspace1.2.2) 
# As expected with a different seed a different subset of combinations are selected

############################
##### Function 1: get_subspaces 

get_subspaces <- function(dimensions, max_spaces, seed){
  all_subspaces <- get_all_subspaces(dimensions)
  
  #### If <max_spaces> is larger than the total number of subspaces
  # then all combinations are returned
  if (max_spaces >= nrow(all_subspaces)) return(all_subspaces)
  
  #### otherwise max_space determins the number of combinations explored
  spaces <- max_spaces
  
  ### Otherwise a subset of combinations choosen randomly are returned
  get_use_subpaces(all_subspaces, spaces, seed)
}

## My tests for get_subspaces
subspaces1 <- get_subspaces(dimensions = 10, max_spaces = 10, seed = 1)
subspaces1 # As expected gets 10 randomly choosen combinations

subspaces2 <- get_subspaces(dimensions = 10, max_spaces = 100, seed =  1)
identical(subspaces2, get_all_subspaces(dimensions = 10))
# As expected if max_spaces exceeds the number of combinations ...
# ... all combinations are returned

###############################################################################
##### Function 2: get_subspace_data
get_subspace_data <- function(data, index, subspace_combinations){
  
  # get the column numbers of the relevant combination out of subspace_combinations
  dim1 <- subspace_combinations[index,1]
  dim2 <- subspace_combinations[index,2] 
  
  ### return the subset of the data containing only these two columns
  data[,c(dim1,dim2)]
}


## my tests: get_subspace_data

grid1 <- matrix(rnorm(5),nrow = 20, ncol = 10)   
#### should still be defined above
# subspaces1 <- get_subspaces(dimensions = 10, max_spaces = 10, seed = 1)
subspace_data1 <- get_subspace_data(data = grid1, index = 1, 
                                   subspace_combinations = subspaces1)
subspace_data1 ## As expected it returns 2 columns from the dataset

subspaces1[1,] # the first row in subspaces1 refers to columns 2 and 5
identical(subspace_data1, grid1[,subspaces1[1,]], grid1[,c(2,5)])
## As expected all approaches access the same two rows of data

###############################################################################


############################
##### Function 0: get_contrasts 
get_contrasts <- function(data, spaces = NULL, deviation = c("ks", "cvm", "tw"),
                          slice = 0.2, draws = 1e2, max_spaces = 4950,
                          seed = NULL) {
  library(checkmate)
  library(goftest)
  # TODO
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
isTRUE(get_contrasts(grid)$deviation < .2)
isTRUE(get_contrasts(line)$deviation > .9)
isTRUE(get_contrasts(fuzzy_line)$deviation <
         get_contrasts(line)$deviation)
contrasts_examples <- get_contrasts(examples)
# all subspaces are covered:
nrow(contrasts_examples) == choose(6, 2)
# 1st column has smaller column numbers, 2nd the higher ones:
all(sort(unique(contrasts_examples[, 1])) == 1:(ncol(examples) - 1))
all(sort(unique(contrasts_examples[, 2])) == 2:ncol(examples))
# rows are sorted high to low according to deviation:
all(rank(contrasts_examples$deviation) == 15:1)
#"line" has highest contrast, "grid" has lowest contrast
all(contrasts_examples[1, 1:2] == c(1, 2))
all(contrasts_examples[15, 1:2] == c(5, 6))
# default deviation is 1 - p-value:
all(contrasts_examples$deviation > 0 & contrasts_examples$deviation < 1)
#-------------------------------------------------------------------------------
# CHECK: yields reproducible results:
identical(get_contrasts(examples, seed = 12121),
          get_contrasts(examples, seed = 12121))
#-------------------------------------------------------------------------------
# CHECK: cleans up inputs:
4
get_contrasts(cbind(grid, NA, 1), seed = 12121) # should trigger warning(s)
identical(get_contrasts(cbind(grid, NA, 1), seed = 12121),
          get_contrasts(grid, seed = 12121))
#-------------------------------------------------------------------------------
# CHECK: accepts (and cleans up) data.frames:
df_examples <- cbind(as.data.frame(examples),
                     some_factor = gl(10, 10))
# this should give a warning:
get_contrasts(df_examples, seed = 12121)
# irrelevant columns are ignored/removed:
identical(get_contrasts(df_examples, seed = 12121),
          get_contrasts(examples, seed = 12121))
#-------------------------------------------------------------------------------
# CHECK: implements different types of deviation measures correctly:
set.seed(122)
# three_d has strong correlation for (1,2), medium for (1,3), zero for (2, 3)
sigma <- matrix(c(1,.9, .4, .9, 1, 0, .4, 0, 1), 3, 3)
three_d <- mvtnorm::rmvnorm(500, sigma = sigma)
# same order of subspaces for different deviations:
!identical(get_contrasts(three_d, deviation = "cvm", seed = 12121),
           get_contrasts(three_d, deviation = "ks", seed = 12121))
identical(get_contrasts(three_d, deviation = "cvm", seed = 12121)[,1:2],
          get_contrasts(three_d, deviation = "ks", seed = 12121)[,1:2])
!identical(get_contrasts(three_d, deviation = "cvm", seed = 12121),
           get_contrasts(three_d, deviation = "tw", seed = 12121))
identical(get_contrasts(three_d, deviation = "cvm", seed = 12121)[,1:2],
          get_contrasts(three_d, deviation = "tw", seed = 12121)[,1:2])
# accepts user defined functions for deviation:
ks_user <- function(conditional, marginal) {
  1 - ks.test(conditional, marginal)$p.value
}
identical(get_contrasts(three_d, deviation = ks_user, seed = 12121),
          get_contrasts(three_d, seed = 12121))
#-------------------------------------------------------------------------------
# CHECK: implements max_spaces arg correctly:
get_contrasts(examples, max_spaces = 2)
#above should give an informative warning that not all sub-spaces are explored.
isTRUE(nrow(get_contrasts(examples, max_spaces = 7)) == 7L)
#reproducible results:
identical(get_contrasts(examples, seed = 12121, max_spaces = 3),
          get_contrasts(examples, seed = 12121, max_spaces = 3))
highdim <- matrix(runif(1e4), ncol = 1e3, nrow = 10)
isTRUE(nrow(get_contrasts(highdim, draws = 1)) == choose(100, 2))
#-------------------------------------------------------------------------------
# CHECK: implements spaces arg correctly:
identical(get_contrasts(three_d, spaces = combn(1:3, 2), seed = 12121),
          get_contrasts(three_d, seed = 12121))
identical(get_contrasts(three_d, spaces = rbind(2, 3))[1, -3],
          5
          c(2, 3))
get_contrasts(three_d, spaces = cbind(c(3, 2), c(2, 3), c(2, 3)))
# above should give an informative warning...
# ... and remove redundant spaces:
identical(get_contrasts(three_d, spaces = rbind(2, 3), seed = 12121),
          get_contrasts(three_d, spaces = cbind(c(3, 2), c(2, 3), c(2, 3)),
                        seed = 12121))
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

