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
all(contrasts_examples[1, 1:2] == c(1, 2)) # is true
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
          get_contrasts(grid, seed = 12121)) 
# is TRUE, with expected warnings
#-------------------------------------------------------------------------------
# CHECK: accepts (and cleans up) data.frames:
df_examples <- cbind(as.data.frame(examples),
                     some_factor = gl(10, 10))
# this should give a warning:
get_contrasts(df_examples, seed = 12121) ## works as expected
# irrelevant columns are ignored/removed:
identical(get_contrasts(df_examples, seed = 12121),
          get_contrasts(examples, seed = 12121)) 
# is True, with expected warnings
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
          get_contrasts(three_d, deviation = "ks", seed = 12121)[,1:2]) 
# doesn't work yet
!identical(get_contrasts(three_d, deviation = "cvm", seed = 12121),
           get_contrasts(three_d, deviation = "tw", seed = 12121)) # is TRUE
identical(get_contrasts(three_d, deviation = "cvm", seed = 12121)[,1:2],
          get_contrasts(three_d, deviation = "tw", seed = 12121)[,1:2]) 
# doen't work yet
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
## Works, and gives warning
isTRUE(nrow(get_contrasts(examples, max_spaces = 7)) == 7L) # is True
#reproducible results:
identical(get_contrasts(examples, seed = 12121, max_spaces = 3),
          get_contrasts(examples, seed = 12121, max_spaces = 3)) # is TRUE
highdim <- matrix(runif(1e4), ncol = 1e3, nrow = 10)
isTRUE(nrow(get_contrasts(highdim, draws = 1)) == choose(100, 2)) # is TRUE
#-------------------------------------------------------------------------------
# CHECK: implements spaces arg correctly:
identical(get_contrasts(three_d, spaces = combn(1:3, 2), seed = 12121),
          get_contrasts(three_d, seed = 12121)) # is TRUE
identical(get_contrasts(three_d, spaces = rbind(2, 3))[1, -3],
          c(2, 3))  
# doesn't Work yet because get_contrast returns a matrix not a vector
get_contrasts(three_d, spaces = cbind(c(3, 2), c(2, 3), c(2, 3)))
# Works as expected, with correct warning
# above should give an informative warning...
# ... and remove redundant spaces:
identical(get_contrasts(three_d, spaces = rbind(2, 3), seed = 12121),
          get_contrasts(three_d, spaces = cbind(c(3, 2), c(2, 3), c(2, 3)),
                        seed = 12121)) 
# is TRUE, with correct warning

#-------------------------------------------------------------------------------
# FAILS: the calls below should all fail with INFORMATIVE, 
# precise error messages
get_contrasts(as.character(grid)) # returns expected error message
get_contrasts(matrix(1, 10, 10)) # returns expected error message
get_contrasts(data.frame(gl(10, 10), gl(5, 20))) 
# returns expected error message
get_contrasts(line, slice = "a") # returns expected error message
get_contrasts(line, slice = 1*NA) # returns expected error message
get_contrasts(line, slice = 0) # returns expected error message
get_contrasts(line, slice = 2) # returns expected error message
get_contrasts(line, slice = c(1, 2)) # returns expected error message
get_contrasts(line, draws = "a") # returns expected error message
get_contrasts(line, draws = 1*NA) # returns expected error message
get_contrasts(line, draws = 0.5) # returns expected error message
get_contrasts(line, draws = c(1, 2)) # returns expected error message
get_contrasts(line, deviation = "sk") # returns expected error message
bs <- function(x1, x2) {
  cat("bs!")
}
get_contrasts(line, deviation = bs) # returns expected error message
get_contrasts(examples, max_spaces = "a") # returns expected error message
get_contrasts(examples, max_spaces = NA) # returns expected error message
get_contrasts(examples, max_spaces = -1) # returns expected error message
get_contrasts(examples, spaces = expand.grid(1:2, 1:3)) 
# returns expected error message
get_contrasts(examples, spaces = combn(1:7, 2))# returns expected error message

