

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

### Funciton for Checking an object is a valid intervals object with 1 interval
test_single_interval <- function(interval){
  name_object <- as.character(substitute(interval))
  
  ### Check object is an Intervals object 
  if (!check_class(interval,"Intervals")) stop(
    paste(name_object,"must be an Intervals object", sep = " "))
  
  ### Check it a valid Intervals objects
  if (!validObject(interval)) # no stop function required
    # because if it is not a valid Intervals object then pre-implemented error
    # messages that fits the type of objects thats given is returned
    
    ### Check it contains a single Interval
    if (!(nrow(interval@.Data) == 1)) stop(
      paste(name_object,"may only contain one Interval", sep = " "))
  
  ### return nothing (function only does something if there is a problem)
}

##### my test for is_single_interval
# pass test
test_single_interval(Int1) # supposed to work -> returns nothing
# fail tests
vec <- c(2,3)
test_single_interval(vec) # returns error: Not of class "Intervals"

#### Doesnt work as expected
test_single_interval(Int3) # returns object -> to may Intervals

fake_int_M <- rbind(c(1,2),c(3,4))
class(fake_int_M) <- "Intervals"
test_single_interval(fake_int_M) # returns error message built into Intervals

fake_int_V <- c(1,2)
class(fake_int_V) <- "Intervals"
test_single_interval(fake_int_V) # returns different error message

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
Int_test1 <- Intervals(c(1,3))
Int_test2 <- Intervals(c(2,4))
Int_test3 <- Intervals(c(4,6))
overlap_interval(Int_test1,Int_test2) # returns expected Interval
overlap_interval(Int_test1,Int_test3) # returns empty interval
overlap_interval(Int_test2,Int_NA1) # returns Interval with NA removed
overlap_interval(Int_NA1,Int_test3) # returns empty interval

# ### Test does naming affect identity -> concusion no
# named_na <- c(NA_real_,NA)
# names(named_na) <- c("one","two")
# named_na_int <- Intervals(named_na)
# identical(Intervals(c(NA_real_,NA)), named_na_int)

############## overlap_per_rectangle get the rectanges of each overlap indivdually
#### Input: list with 4 endpoint vectors, for height and Width of rectangles A and B
#### Output: list with 2 endpoint vectors for height and width of the overlap rectanle

overlap_per_rectangle <- function(endpoints_list) {
  
  #### Get the overlap respectively for width and height
  # endpoint list contains vectors of endpoints
  # So these must be made to Intervals
  O_int_width <- overlap_interval(Intervals(endpoints_list[["A_width"]]),
                                  Intervals(endpoints_list[["B_width"]]))
  O_int_height <- overlap_interval(Intervals(endpoints_list[["A_height"]]),
                                   Intervals(endpoints_list[["B_height"]]))
  
  # If in either direction there is no overlap return NAs for all endpoints
  # This comparasion was chosen because overlap_interval returns Interval()
  # when there is no overlap
  if(identical(O_int_width, Intervals()) || identical(O_int_height, Intervals())){
    return(list(width = c(NA,NA), height = c(NA,NA)))
  }
  
  # else return the endpoint of the overlaps
  list(width = O_int_width@.Data, height = O_int_height@.Data)
  
}

# my Test for overlap_per_rectangle
end_list1 <- list(A_width = c(1,3), A_height = c(4,5), 
                  B_width = c(1,6), B_height = c(4,6))
(overlap_per_rectangle(end_list1)) # returns non NA Coordiantes as expected

end_list2 <- list(A_width = c(1,1), A_height = c(4,5), 
                  B_width = c(2,6), B_height = c(4,6))
(overlap_per_rectangle(end_list2)) # returns all NAs as expected

end_list3 <- list(A_width = c(NA,3), A_height = c(4,5), 
                  B_width = c(2,6), B_height = c(4,6))
(overlap_per_rectangle(end_list3)) # Handles NAs as expected

############# Test valid Rectangles function
test_valid_Rectangles <- function(object) {
  if(!check_class(object, "Rectangles")) stop(paste(as.character(bquote(object))
                                              , "must be a Rectangles Object"))
  if(!validObject(object)) stop(paste(as.character(bquote(object))
                                , "must be a valid Rectangles Object"))
}
## my tests for test_valid_Rectangles
not_rec <- c(1,2)
test_valid_Rectangles(not_rec)
not_rec2 <- not_rec
class(not_rec2) <- "Rectangles" 
test_valid_Rectangles(not_rec2) # error message returned

############################### Overlap ########################################
overlap <- function(rectangle_A, rectangle_B){
  ## Rectangles objects must be valid rectanges and have the same size
  test_valid_Rectangles(rectangle_A)
  test_valid_Rectangles(rectangle_B)
  if(!identical(nrow(rectangle_A@x@.Data),nrow(rectangle_B@x@.Data))) stop(
    "Only Rectangles objects of the same size can be compaired")
  
  ## Get the coordinates of the endpoint of all rectangeles in A and B
  A_width_matrix <- rectangle_A@x@.Data
  A_height_matrix <- rectangle_A@y@.Data
  B_width_matrix <- rectangle_B@x@.Data
  B_height_matrix <- rectangle_B@y@.Data
  
  # get the number of rectangles being compaired
  n <- nrow(A_width_matrix) 
  
  # Matrice of endpoint for the overlap rectangles  
  O_width_matrix <- matrix(NA, nrow = n, ncol = 2)
  O_height_matrix <- matrix(NA, nrow = n, ncol = 2)
  
  ##### Fill with the endpoint of each indiviudal overlap matrix
  for(i in 1:n){
    # Get list with all endpoint coordinates
    endpoints_list <- list(A_width = A_width_matrix[i,], 
                          A_height = A_height_matrix[i,],
                          B_width = B_width_matrix[i,], 
                          B_height =B_height_matrix[i,])
    # Get endpoints of overlap rectangles
    o_endpoint_list <- overlap_per_rectangle(endpoints_list)
    # write endpoints into matrices
    O_width_matrix[i,] <- o_endpoint_list[["width"]]
    O_height_matrix[i,] <- o_endpoint_list[["height"]]
  }
  
  #### Make a new Rectanles object containing each overlap rectangle
  Rectangles(Intervals(O_width_matrix), Intervals(O_height_matrix))
  
}

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
11
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
