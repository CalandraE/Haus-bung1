library(intervals)
source("intervals-rectangles-class.R")
source("intervals-rectangles-methods.R")

#-------------------------------------------------------------------------------

rect <- Rectangles(
  x = rbind(c(0, 1), c(-1, 2), c(0, 1)),
  y = rbind(c(0, 2), c(1, 2), c(-Inf, Inf)))
rect_na <- Rectangles(c(0, NA), c(1, 2))
rect_line <- Rectangles(c(0, 0), c(1, 2))

#-------------------------------------------------------------------------------
# CHECK: show-method works as expected:
rect

#-------------------------------------------------------------------------------
# CHECK: size-method works as expected:
all.equal(size(rect), c(2, 3, Inf))
all.equal(size(rect_na), NA_real_)
all.equal(size(rect_line), 0)
