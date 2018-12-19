

############################### Plan: Overlap ###############################

### Input: 2 Rectangles objects
# In all test cases they contain only one rectangle

# should fail for matrix or vector inputs 
# -> assert Rectangles object using check_class()
# also check validty of rectangles objects with validObject()

# should fail for only one Rectangels object
# -> insist on two inputs


#### Get overlap in the Width and height intervals seperately
## get Intervals out of retangles
## find Overlapping intervals

####( Deal with the NA cases (minimal possible overlap interval)
## Falls vorhanden Interval grenze in den Anderen Interval liegt dann 
## den NA grenze gleich das vorhande stellen (breite 0)
## eg. ([NA,1] x [3,4]) intersect ([1,2] x [3,4])
## 1 liegt in [1,2] -> NA = 1 anhmen -> Rectangles(c(1,1), c(3,4))
# )

#### Test if both intervals overlap
## If it does make a new Rectangles object out of the overlap intervals
## If not return empty Rectangles

### Output: Rectangles object (overlap of input)

########### Investigate funcitons in Intervals
?Intervals

### Potentally useful Funktions:
# interval_union 
# reduce # removes Intervals with na end points
# which_nearest
# interval_intersection
# contract
getMethod("interval_union", "Intervals_virtual") # uses reduce
getMethod("reduce", "Intervals_virtual")
getMethod("interval_intersection", "Intervals_virtual") # uses interval_complement
getMethod("interval_complement", "Intervals_virtual")

## Investigate behaviour of Intervals Methods

# Single Interval
Int1 <- Intervals(c(1,2))
Int2 <- Intervals(c(2,5))
Int3 <- Intervals(c(3,5))
interval_union(Int1,Int2) # merges them
interval_intersection(Int1,Int2) # gets intersection -> has class Intervals
interval_intersection(Int1,Int3) # returns an empty Intervals object

### Note: This is not the same things as returning [NA,NA]
identical(interval_intersection(Int1,Int3),Intervals(c(NA_real_,NA)))
### must be modified!!!!!
### resulting rectangles must be empty

# Multiples Intervals
Int3 <- Intervals(rbind(c(1,2),c(2,4)))
Int4 <- Intervals(rbind(c(2,3),c(1,5)))
interval_union(Int3,Int4) # only seems to merge the second Interval pair
interval_intersection(Int3,Int4) # Also doesn't work as expected

#### conclusion: Only use methods of Intervals indidually

### Check behaviour of interval_intersection on NA
Int_NA1 <- Intervals(c(NA,2)) 
Int_NA2 <- Intervals(c(2,NA))
# Int_NA_both <- Intervals(c(NA,NA)) # not allowed by Intervals
interval_intersection(Int1,Int_NA1) # no intersections
interval_intersection(Int1,Int_NA2) # no intersections
## -> must be modified before use

## is.na finds wheather an NA is contained in the Intervals bounderies
is.na(Int_NA1) # TRUE
is.na(Int_NA2) # TRUE -> place of NA is not returned
is.na(Int1) # correctly identifies that neither end points is NA


############################### overlap_interval ###############################
#### Function get the Interval that is the overlap of 2 Intervals

### First here is the Function is needed to deal with cases were there is 1 NA

######### Plan for process_NA_endpoint()
# find na_endpoint
# check if non_na endpoint is in other interval
###### if it isn't return empty Intervals
# otherwise overwrite na endpoint with the the non_na value

process_NA_endpoint <- function(na_interval, non_na_interval){
  # Test inputs are valid
  test_single_interval(na_interval)
  test_single_interval(non_na_interval)
  
  # must find which endpoint
  na_end_postion <- which(is.na(na_interval@.Data))
  non_na_end_postion <- which(!is.na(na_interval@.Data))
  
  non_na_end_value <- na_interval@.Data[,non_na_end_postion]
  
  # get start and end point of the non_na_interval 
  start <- non_na_interval@.Data[,1]
  end <- non_na_interval@.Data[,2]
  
  # if the non_na_endpoint is NOT in the other interval return empty interval
  if ((start > non_na_end_value) || (non_na_end_value > end)) return(Intervals())
  
  # otherwise overwrite na endpoint with the other value
  na_interval@.Data[,na_end_postion] <- non_na_end_value
  
  # the na_interval is therefore inside the non_na_interval
  # na_interval is the same as interval_intersection(na_interval,non_na_interval)
  # so to be more efficient the now modified na_interval 
  
  na_interval
}

# my tests process_NA_endpoint 
# pass tests
process_NA_endpoint(Int_NA1, Int1) # works: returns Interval width 0
Int5 <- Intervals(c(10,12))
process_NA_endpoint(Int_NA1, Int5) # works: returns empty Interval
# fail tests
process_NA_endpoint(Int_NA1, c(10,11)) # gives informative error message


overlap_interval <- function(interval_A,interval_B) {
  ###### Check objects given are Intervals
  test_single_interval(interval_A)
  test_single_interval(interval_B)
  # if nothing happens then the objects have the write form/type
  # otherwise an informative error message is returned
  
  ###### First case: A and B both contain no NA Endpoints
  if (!is.na(interval_A) && !is.na(interval_B)) {
    # the intersection can be calculated using the interval_intersection function
    # from the Intervals package 
    # -> it returns an Intervals class object so further change is needed
    return(interval_intersection(interval_A,interval_B))
  }
  
  ###### Second case: A and B both contain an NA end point
  # Return empty Intervals objects
  if (is.na(interval_A) && is.na(interval_B)) Intervals()
  
  ###### third case: A or B has 2 NA end points
  ## ie is identical to [NA,NA]
  # Note: naming the vector does not affect identity (I tested this)
  # Return empty Intervals objects
  
  if (identical(Intervals(c(NA_real_,NA)), interval_A) && 
      identical(Intervals(c(NA_real_,NA)), interval_B)) return(Intervals())
  
  ###### fourth case: A xor B has 1 NA endpoint
  # must find which one of the intervals (A or B) has the NA endpoint
  
  if (is.na(interval_A)) return(process_NA_endpoint(na_interval = interval_A,
                                                    non_na_interval = interval_B))
  
  # if interval_A is not the one with an na Endpoint it must be interval_B
  # as all other combinations have return functions
  
  # If the funtion hasn't returned at some earlier point this case is returned
  process_NA_endpoint(na_interval = interval_B, non_na_interval = interval_A)
}

## my tests for overlap_interval()
Int1
Int2
overlap_interval(Int2,Int3) # returns expected Interval
overlap_interval(Int1,Int3) # returns empty interval
overlap_interval(Int2,Int_NA1) # returns Interval with NA removed
overlap_interval(Int_NA1,Int3) # returns empty interval

# ### Test does naming affect identity -> concusion no
# named_na <- c(NA_real_,NA)
# names(named_na) <- c("one","two")
# named_na_int <- Intervals(named_na)
# identical(Intervals(c(NA_real_,NA)), named_na_int)

############################### get_overlap_vector #########################
### Idea: Evaluate whether each pair rectangles in the rectangles overlap
# by testing them seperately and returning the x and y endpoints for each 
# overlap rectangle
# these endpoints are colleccted into x and y matrices (function gives a list)
# Using these matrices a new Rectanges object is made

get_overlap_vectors <- function(row, A_width, A_height, B_width, B_height) {
  
  # make an Interval using the "row" row of the matrixes
  # get the overlap of the pairs of width and height intervas

  width_overlap <- overlap_interval(Intervals(A_width[row,]), 
                                    Intervals(B_width[row,]))
  
  height_overlap <- overlap_interval(Intervals(A_height[row,]), 
                                     Intervals(B_height[row,]))
  
  # If the width or height Intervals don't overlap no rectange is possible
  # in that case the end points returned are all NA
  if (identical(Intervals(), width_overlap) || 
       identical(Intervals(), height_overlap)) return(
         list(x = c(NA,NA), y = c(NA,NA)))
 
  # otherwise the returned endpoints of the height and width matrices
  list(x = width_overlap@.Data, y = height_overlap@.Data)
}

# My test get_overlap_vector
test_A_width <- rbind(c(1,2),c(3,4))
test_A_height <- rbind(c(10,12),c(13,14))
test_B_width <- rbind(c(1,4),c(5,6))
test_B_height <- rbind(c(10,12),c(13,14))
get_overlap_vectors(row = 1, test_A_width, test_A_height, 
                   test_B_width, test_B_height ) # Returns endpoints as expected
get_overlap_vectors(row = 2, test_A_width, test_A_height, 
                   test_B_width, test_B_height ) # Returns NAs as expected

get_overlap_vectors(row = 1, test_A_width, test_A_height, 
                   test_B_width, test_B_height )[["x"]]

############################### Overlap ########################################
overlap <- function(rectangle_A, rectangle_B){
  ## INCLUDE TESTS
  
  # Rectangles A and B must contain the same number of Rectanges
  if (!(nrow(rectangle_A@x@.Data) ==  nrow(rectangle_B@x@.Data))) {
    stop("Rectangles_A and Rectangles_B must contain the same number of
         individual Rectangles")
  }
  # number of Rectangles
  n <- nrow(rectangle_A@x@.Data)
  
  ######### Get the overlap of each rectangle seperately
  ### First the matrices of all the Intervals must be extracted from Rectangles
  A_width <- rectangle_A@x@.Data
  A_height <- rectangle_A@y@.Data
  B_width <- rectangle_B@x@.Data
  B_height <- rectangle_B@y@.Data
  
  ### matrices of endpoints for the overlap rectanges
  width_overlap <- matrix(NA, nrow = n, ncol = 2)
  height_overlap <- matrix(NA, nrow = n, ncol = 2)
  
  ### fill matrices 
  for (i in 1:n) {
    # get list containing the vectors of the endpoints of the overlap Rectangle
    overlap_vectors <- get_overlap_vectors(row = i, A_width, A_height, 
                                            B_width, B_height)
    # get width and height endpoints
    
    # use as.numeric to convert NAs into numeric NAs
    width_overlap[i,] <- as.numeric(overlap_vectors[["x"]])
    height_overlap[i,] <- as.numeric(overlap_vectors[["y"]])
  }
  
  
  ## If either in the width or height direction 
  # there is no overlap return an empty Rectangles object
  # This comparasion was chosen because overlap_interval returns Interval()
  # when there is no overlap
  
  # if (identical(Intervals(), width_overlap) || 
  #     identical(Intervals(), height_overlap)) return(
  #       Rectangles(Intervals(),Intervals()))
  # 
  ## Otherwise return a Rectangles object made of the overlap intervals
  Rectangles(width_overlap, height_overlap)
    
}

# my tests 
Test_Rectangles_A <- Rectangles(Intervals(test_A_width),Intervals(test_A_height))
Test_Rectangles_B <- Rectangles(Intervals(test_B_width),Intervals(test_B_height))
overlap(Test_Rectangles_A,Test_Rectangles_B)

A <- Rectangles(c(0, 1), c(0, 2))
B <- Rectangles(c(2, 3), c(4, 6))
rectangles_A <- A
rectangles_B <- B
overlap(rectangles_A,rectangles_B)
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
  Rectangles(c(0, 1), c(1, 2))) ### Returns TRUE as expected
# ([0, 1]×[0, 1]) ∩ ([.3, .5]×[.3, .5]) = ([.3, .5]×[.3, .5])
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(.3, .5), c(.3, .5))),
  Rectangles(c(.3, .5), c(.3, .5))) ### Returns TRUE as expected
# ([0, 1]×[0, 2]) ∩ ([2, 3]×[4, 6]) = Ø
identical(overlap(Rectangles(c(0, 1), c(0, 2)),
                  Rectangles(c(2, 3), c(4, 6))),
          rect_empty) #### DOESNT WORK YET!!!!!!!
# overlap is symmetric:
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(0, 2), c(1, 2))),
  overlap(Rectangles(c(0, 2), c(1, 2)),
          Rectangles(c(0, 1), c(0, 2)))) ### Returns TRUE as expected
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(.3, .5), c(.3, .5))),
  overlap(Rectangles(c(.3, .5), c(.3, .5)),
          Rectangles(c(0, 1), c(0, 2)))) ### Returns TRUE as expected
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(2, 3), c(4, 6))),
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(2, 3), c(4, 6)))) ### Returns TRUE as expected
#-------------------------------------------------------------------------------
# CHECK: basic functionality for sets of rectangles
identical(
  overlap(rect, rect),
  rect) #### DOESNT WORK YET!!!!!!!
identical(
  overlap(Rectangles(c(-Inf, Inf), c(-Inf, Inf)), rect),
  rect) #### DOESNT WORK YET!!!!!!!
identical(
  overlap(Rectangles(c(-Inf, Inf), c(-Inf, Inf)), rect),
  overlap(rect, Rectangles(c(-Inf, Inf), c(-Inf, Inf)))) # Returns TRUE
#-------------------------------------------------------------------------------
# CHECK: deals with NAs sensibly:
# returns the *smallest* rectangle that is guaranteed to be in the intersection:
# ([0, 1]×[0, 1]) ∩ ([?, .5]×[.5, 1]) ⊇ ([.5, .5]×[.5, 1])
identical(
  overlap(Rectangles(c(0, 1), c(0, 1)),
          Rectangles(c(NA, .5), c(.5, 1))),
  Rectangles(c(.5, .5), c(.5, 1))) # Returns TRUE

# ([0, 1]×[0, 2]) ∩ ([2, ?]×[1, 2]) = ∅ (no overlap at all in x direction)
identical(
  overlap(Rectangles(c(0, 1), c(0, 2)),
          Rectangles(c(2, NA), c(1, 2))),
  rect_empty) ### Returns TRUE as expected
# ([0, 1]×[0, ?]) ∩ ([?, .5]×[.5, 1]) = ∅ (!)
# possibly no overlap on y-axis, and then no overlap at all --> empty set
identical(
  overlap(Rectangles(c(0, 1), c(0, NA)),
          Rectangles(c(NA, .5), c(.5, 1))),
  rect_empty) ### Returns TRUE as expected
#-------------------------------------------------------------------------------
# FAILS:
# The following calls should fail with INFORMATIVE & precise error messages:
overlap(rect) # Returns error message stating rectangle_B missing
overlap(rect, cbind(c(0, 0),c(1, 1)))
overlap(c(0,1), cbind(c(0, 0),c(1, 1)))
overlap(rect,
        Rectangles(cbind(c(0, 0),c(1, 1)), cbind(c(0, 0),c(1, 1))))
### doesnt work yet
