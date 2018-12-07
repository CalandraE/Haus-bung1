library(intervals)
source("intervals-rectangles-class.R")
source("intervals-rectangles-methods.R")
source("intervals-rectangles-overlap.R")

#-------------------------------------------------------------------------------

rect <- Rectangles(
  x = rbind(c(0, 1), c(-1, 2), c(0, 1)),
  y = rbind(c(0, 2), c(1, 2), c(-Inf, Inf)))
rect_empty <- Rectangles(c(NA_real_, NA), c(NA_real_, NA))

#-------------------------------------------------------------------------------
# CHECK: basic functionality
# ([0, 1]×[0, 2]) ∩ ([0, 2]×[1, 2]) = ([0, 1]×[1, 2])
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(0, 2), c(1, 2))),
  Rectangles(c(0, 1), c(1, 2)))
# ([0, 1]×[0, 1]) ∩ ([.3, .5]×[.3, .5]) = ([.3, .5]×[.3, .5])
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(.3, .5), c(.3, .5))),
  Rectangles(c(.3, .5), c(.3, .5)))
# ([0, 1]×[0, 2]) ∩ ([2, 3]×[4, 6]) = Ø
identical(overlap(Rectangles(c(0, 1), c(0, 2)),
                  Rectangles(c(2, 3), c(4, 6))),
          rect_empty)
# overlap is symmetric:
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(0, 2), c(1, 2))),
  overlap(Rectangles(c(0, 2), c(1, 2)),
          Rectangles(c(0, 1), c(0, 2))))
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(.3, .5), c(.3, .5))),
  overlap(Rectangles(c(.3, .5), c(.3, .5)),
          Rectangles(c(0, 1), c(0, 2))))
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(2, 3), c(4, 6))),
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(2, 3), c(4, 6))))

#-------------------------------------------------------------------------------
# CHECK: basic functionality for sets of rectangles
identical(
  overlap(rect, rect),
  rect)
identical(
  overlap(Rectangles(c(-Inf, Inf), c(-Inf, Inf)), rect),
  rect)
identical(
  overlap(Rectangles(c(-Inf, Inf), c(-Inf, Inf)), rect),
  overlap(rect, Rectangles(c(-Inf, Inf), c(-Inf, Inf))))

#-------------------------------------------------------------------------------
# CHECK: deals with NAs sensibly:
# returns the *smallest* rectangle that is guaranteed to be in the intersection:
# ([0, 1]×[0, 1]) ∩ ([?, .5]×[.5, 1]) ⊇ ([.5, .5]×[.5, 1])
identical(
  overlap(Rectangles(c(0, 1), c(0, 1)),
          Rectangles(c(NA, .5), c(.5, 1))),
          Rectangles(c(.5, .5), c(.5, 1)))
# ([0, 1]×[0, 2]) ∩ ([2, ?]×[1, 2]) = ∅ (no overlap at all in x direction)
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(2, NA), c(1, 2))),
  rect_empty)
# ([0, 1]×[0, ?]) ∩ ([?, .5]×[.5, 1]) = ∅ (!)
# possibly no overlap on y-axis, and then no overlap at all --> empty set
identical(
  overlap(Rectangles(c(0, 1), c(0, NA)),
          Rectangles(c(NA, .5), c(.5, 1))),
  rect_empty)

#-------------------------------------------------------------------------------
# FAILS:
# The following calls should fail with INFORMATIVE & precise error messages:
overlap(rect)
overlap(rect, cbind(c(0, 0),c(1, 1)))
overlap(c(0,1), cbind(c(0, 0),c(1, 1)))
overlap(rect,
        Rectangles(cbind(c(0, 0),c(1, 1)), cbind(c(0, 0),c(1, 1))))
