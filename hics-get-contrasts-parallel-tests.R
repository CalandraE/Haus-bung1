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
isTRUE(get_contrasts_parallel(grid)$deviation < .2) 
# false but doesn't work in get_contrasts either
isTRUE(get_contrasts_parallel(line)$deviation > .9)
isTRUE(get_contrasts_parallel(fuzzy_line)$deviation <
         get_contrasts_parallel(line)$deviation)
contrasts_examples <- get_contrasts_parallel(examples)
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
identical(get_contrasts_parallel(examples, seed = 12121),
          get_contrasts_parallel(examples, seed = 12121))
#-------------------------------------------------------------------------------
# CHECK: cleans up inputs:

get_contrasts_parallel(cbind(grid, NA, 1), seed = 12121) # should trigger warning(s)
identical(get_contrasts_parallel(cbind(grid, NA, 1), seed = 12121),
          get_contrasts_parallel(grid, seed = 12121))
#-------------------------------------------------------------------------------
# CHECK: accepts (and cleans up) data.frames:
df_examples <- cbind(as.data.frame(examples),
                     some_factor = gl(10, 10))
# this should give a warning:
get_contrasts_parallel(df_examples, seed = 12121)
# irrelevant columns are ignored/removed:
identical(get_contrasts_parallel(df_examples, seed = 12121),
          get_contrasts_parallel(examples, seed = 12121))
#-------------------------------------------------------------------------------
# CHECK: implements different types of deviation measures correctly:
set.seed(122)
# three_d has strong correlation for (1,2), medium for (1,3), zero for (2, 3)
sigma <- matrix(c(1,.9, .4, .9, 1, 0, .4, 0, 1), 3, 3)
three_d <- mvtnorm::rmvnorm(500, sigma = sigma)
# same order of subspaces for different deviations:
!identical(get_contrasts_parallel(three_d, deviation = "cvm", seed = 12121),
           get_contrasts_parallel(three_d, deviation = "ks", seed = 12121))
identical(get_contrasts_parallel(three_d, deviation = "cvm", seed = 12121)[,1:2],
          get_contrasts_parallel(three_d, deviation = "ks", seed = 12121)[,1:2])
!identical(get_contrasts_parallel(three_d, deviation = "cvm", seed = 12121),
           get_contrasts_parallel(three_d, deviation = "tw", seed = 12121))
identical(get_contrasts_parallel(three_d, deviation = "cvm", seed = 12121)[,1:2],
          get_contrasts_parallel(three_d, deviation = "tw", seed = 12121)[,1:2])
# accepts user defined functions for deviation:
ks_user <- function(conditional, marginal) {
  1 - ks.test(conditional, marginal)$p.value
}
identical(get_contrasts_parallel(three_d, deviation = ks_user, seed = 12121),
          get_contrasts_parallel(three_d, seed = 12121))
#-------------------------------------------------------------------------------
# CHECK: implements max_spaces arg correctly:
get_contrasts_parallel(examples, max_spaces = 2)
#above should give an informative warning that not all sub-spaces are explored.
isTRUE(nrow(get_contrasts_parallel(examples, max_spaces = 7)) == 7L)
#reproducible results:
identical(get_contrasts_parallel(examples, seed = 12121, max_spaces = 3),
          get_contrasts_parallel(examples, seed = 12121, max_spaces = 3))
highdim <- matrix(runif(1e4), ncol = 1e3, nrow = 10)
isTRUE(nrow(get_contrasts_parallel(highdim, draws = 1)) == choose(100, 2))
#-------------------------------------------------------------------------------
# CHECK: implements spaces arg correctly:
identical(get_contrasts_parallel(three_d, spaces = combn(1:3, 2), seed = 12121),
          get_contrasts_parallel(three_d, seed = 12121))
identical(get_contrasts_parallel(three_d, spaces = rbind(2, 3))[1, -3],
          c(2, 3))  
# doesn't Work yet because get_contrast returns a matrix not a vector
get_contrasts_parallel(three_d, spaces = cbind(c(3, 2), c(2, 3), c(2, 3)))
# above should give an informative warning...
# ... and remove redundant spaces:
identical(get_contrasts_parallel(three_d, spaces = rbind(2, 3), seed = 12121),
          get_contrasts_parallel(three_d, spaces = cbind(c(3, 2), c(2, 3), c(2, 3)),
                        seed = 12121))
#-------------------------------------------------------------------------------
# FAILS: the calls below should all fail with INFORMATIVE, precise error messages
get_contrasts_parallel(as.character(grid))
get_contrasts_parallel(matrix(1, 10, 10))
get_contrasts_parallel(data.frame(gl(10, 10), gl(5, 20)))
get_contrasts_parallel(line, slice = "a")
get_contrasts_parallel(line, slice = 1*NA)
get_contrasts_parallel(line, slice = 0)
get_contrasts_parallel(line, slice = 2)
get_contrasts_parallel(line, slice = c(1, 2))
get_contrasts_parallel(line, draws = "a")
get_contrasts_parallel(line, draws = 1*NA)
get_contrasts_parallel(line, draws = 0.5)
get_contrasts_parallel(line, draws = c(1, 2))
get_contrasts_parallel(line, deviation = "sk")
bs <- function(x1, x2) {
  cat("bs!")
}
get_contrasts_parallel(line, deviation = bs)
get_contrasts_parallel(examples, max_spaces = "a")
get_contrasts_parallel(examples, max_spaces = NA)
get_contrasts_parallel(examples, max_spaces = -1)
get_contrasts_parallel(examples, spaces = expand.grid(1:2, 1:3))
get_contrasts_parallel(examples, spaces = combn(1:7, 2))
