library(intervals)
source("intervals-rectangles-class.R")

#-------------------------------------------------------------------------------
# CHECK:  Rectangles creates a Rectangles S4-object,
#         with slots x and y of class Intervals.
test_rect <- Rectangles(Intervals(c(0, 1)), Intervals(c(1, 2)))
is(test_rect, "Rectangles")
is(test_rect@x, "Intervals")
is(test_rect@y, "Intervals")
identical(test_rect@x@.Data, t(matrix(c(0, 1))))
identical(test_rect@y@.Data, t(matrix(c(1, 2))))

#-------------------------------------------------------------------------------
# CHECK: constructor works for Intervals & corresponding vector or matrix inputs:
unit_rect <- Rectangles(Intervals(c(0, 1)), Intervals(c(0, 1)))
identical(
  unit_rect,
  Rectangles(c(0, 1), c(0, 1)))
identical(
  Rectangles(Intervals(rbind(0:1, 1:2)), Intervals(rbind(0:1, 1:2))),
  Rectangles(rbind(0:1, 1:2), rbind(0:1, 1:2)))

#-------------------------------------------------------------------------------
# FAILS:
# The following calls to Rectangles should fail with INFORMATIVE & precise
#   error messages:
Rectangles(c(-1, -2), c(0, 1))
Rectangles(Intervals(c(0, 1), closed = FALSE), Intervals(c(0, 1)))
Rectangles(Intervals(c(0, 1), type = "Z"), Intervals(c(0, 1)))
Rectangles(Intervals(c(0, 1), type = "Z", closed),
           Intervals(c(0, 1), type = "Z"))
