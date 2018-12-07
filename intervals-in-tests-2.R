# TESTS: %in% for S4-intervals

source("intervals-in.R")
library(intervals)

#-------------------------------------------------------------------------------
# set up example data:

(ints <- Intervals(cbind(0:1, 1:2)))
(ints_open <- Intervals(cbind(0:1, 1:2), closed = FALSE))
(sets <- Intervals(cbind(c(-5, 0, 5), c(0, 5, Inf)),
                  closed = c(TRUE, FALSE), type = "Z"))
(empty <- Intervals(c(0, 0), closed = FALSE))
(na <- Intervals(c(0, NA), closed = TRUE))

#-------------------------------------------------------------------------------
#CHECK: basic functionality:

all.equal(1 %in% ints[1], TRUE)
all.equal(1 %in% ints_open[1], FALSE)
all.equal(NA %in% ints[1], NA)
all.equal(NA %in% na, NA)
all.equal(0 %in% na, TRUE)
all.equal(0 %in% empty, FALSE)

#-------------------------------------------------------------------------------
#CHECK: vector inputs:

all.equal(c(-1, .5, 1, 0) %in% ints[1],
          c(FALSE, TRUE, TRUE, TRUE))
all.equal(c(-1, .5, 1, 0) %in% ints_open[1],
          c(FALSE, TRUE, FALSE, FALSE))
all(c(.5, 1.1) %in% ints)
all.equal(.5 %in% ints,
          c(TRUE, FALSE))
all.equal(5 %in% sets,
          c(FALSE, FALSE, TRUE))
all.equal(0.5 %in% sets,
          c(FALSE, FALSE, FALSE))

#-------------------------------------------------------------------------------
#CHECK: dealing with unsuitable/weird inputs:

"a" %in% ints #should fail or return FALSE? explain pros & cons of your decision.

# The calls below should fail with informative error messages:
c(.5, 1.1, 2) %in% ints #should fail
matrix(0, 2, 2) %in% ints #should fail
list(0, 2, 2) %in% ints #should fail

#-------------------------------------------------------------------------------
#CHECK: deal with open and closed intervals:

(ints_all <- c(ints, ints_open))

all.equal(1 %in% ints_all,
          c(TRUE, TRUE, FALSE, FALSE))
all.equal(c(0, 2, 0, 2) %in% ints_all,
          c(TRUE, TRUE, FALSE, FALSE))
all.equal(c(-1, 1, 0.5, 2) %in% ints_all,
          c(FALSE, TRUE, TRUE, FALSE))
