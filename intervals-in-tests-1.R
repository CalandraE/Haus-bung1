# TESTS: %in% for base-S3-objects

source("intervals-in.R")
# make base-version of %in% available for direct comparisons:
`%base_in%` <- base::`%in%`

all.equal(1:5 %in% c(1, 3, 5, 9),
          1:5 %base_in% c(1, 3, 5, 9))
all.equal(c("a", "b", "c") %in% c("a", "b"),
          c("a", "b", "c") %base_in% c("a", "b"))
all.equal(c("a", "b", "c") %in% list(c("a", "b", "c"), "a"),
          c("a", "b", "c") %base_in% list(c("a", "b", "c"), "a"))
