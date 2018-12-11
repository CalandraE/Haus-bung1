

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

############################### Overlap ########################################
overlap <- function(rectangle_A, rectangle_B){
  ## INCLUDE TESTS
  
  ## Get Overlap of Intervals of Width and height
  width_overlap <- overlap_interval(rectangle_A@x,rectangle_B@x)
  height_overlap <- overlap_interval(rectangle_A@y,rectangle_B@y)
  
  ## If either in the width or height direction 
  # there is no overlap return empty Rectangles
  # This comparasion was chosen because overlap_interval returns Interval()
  # when there is no overlap
  
  ###### MUST MODIFY SO THAT EMPTY RECTANGES ARE POSSIBLE
  if (identical(Intervals(), width_overlap) && 
      identical(Intervals(), height_overlap)) return(
        Rectangles(Intervals(),Intervals()))
    
}
