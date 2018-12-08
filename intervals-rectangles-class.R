

################ A3: Wrecked & Tangled ########################

#install.packages("styler") ## MUST REMOVE BEFORE SUBMISSION
library(styler) ## MUST REMOVE BEFORE SUBMISSION
###### a)
# Plan:
# make class Rectanges
# slots: 'x' Width (Intervals) and 'y' height (Intervals)
# validity: height and width must have the same number of entries
# 
# Make contructor 
# if height and width entries aren't intervals to begin with it must 
# convert them to intervals
# must include valdity checking to give informative error messages
# incase type check fails

# Write checks were possible into external functions
#install.packages("checkmate")
library(checkmate)


install.packages("intervals")
library(intervals)
#source("intervals-rectangles-class.R")
?intervals

### Define Rectanges class
setClass("Rectangles",
         slots = c( # height and width are given by intervals
           x = "Intervals",
           y = "Intervals"),
         prototype = list(
           # closed = TRUE because it was specifed in the quesions that 
           # only closed intervals are acceptated
           x = new("Intervals", closed = TRUE),
           y = new("Intervals", closed = TRUE)
         ),
         validity = function(object){
           # create list of error message to be filled later
           error_list <- list()
           
           #### Number of hight and width intervals must match to make rectangles
           # get the number of height and width intervals
           num_x_interverals <- nrow(object@x@.Data) # number of rows in the matrix defining interval x
           num_y_interverals <- nrow(object@y@.Data)
           # Test and if they are not the same add the error message to the list 
           if (!(num_x_interverals == num_y_interverals)) {
             error_list[["length_match"]] <- "The number of height and width intervals must be the same"
           }
           
           #### Intervals must be closed
           if (!check_closed(object, "x")) error_list[["x closed"]] <- "x must be closed"
           if (!check_closed(object, "y")) error_list[["y closed"]] <- "y must be closed"
           
           ### Intervals must not have type Z (Warning there is a double negative: not not type z means it is type z)
           if (!check_type_not_Z(object, "x")) error_list[["x is type Z"]] <- "x must not be type Z"
           if (!check_type_not_Z(object, "y")) error_list[["y is type Z"]] <- "y must not be type Z"
           
           # Return error_list unless its empty
           if (!(length(error_list) == 0)) error_list else TRUE
         }
)

# Funktions used in Validator
check_closed <- function(object, slot){
  
  if (!(class(slot(object, slot)) == "Intervals")) stop("check_closed() can only be used on Interval objects")
  
  # get logical vector of whether the interval of the object is closed
  interval_closed <- slot(slot(object, slot), "closed") # aka object@slot@closed
  all(interval_closed == TRUE) # return logical value of weather all intervals are closed
}

check_type_not_Z <- function(object, slot){
  assert_character(slot) # slot entries must be character
  if (!(class(slot(object, slot)) == "Intervals")) stop("check_type() can only be used on Interval objects")
  # get the type of the interval 
  interval_type <- slot(slot(object, slot),"type") # aka object@slot@type
  
  !(identical(interval_type, "Z")) # If the interval type is Z return TRUE
}


### check_class funktion to check wheather the class of a object is the one expected
check_class <- function(object, expected_class) {
  # If the object doesn't have the expected class FALSE
  # Other wise TRUE
  if (!(class(object) %in% expected_class)) { # 'in' allows for mulitple accepted classes
    FALSE
  } else TRUE
}

### Function get_start_before_end  return TRUE if
# the first intveral value is smaller than the second
get_start_before_end <- function(interval_vec){
  if (NA %in% interval_vec) return(NA) # return NA if a value is missing
  if (interval_vec[2] < interval_vec[1]) TRUE
  else FALSE
}

## test get_start_before_end
get_start_before_end(c(1,NA)) # returns NA as expected
get_start_before_end(c(2,3)) # returns false as expected
get_start_before_end(c(2,-5))# returns true as expected

## class_error

# # might be useful later
# check_class_error_list <- function(object, slot_type_list){
#   # intiate list
#   error_list <- list()
#   for (i in 1:length(slot_type_list)) {
#     class_correct <- check_class(slot(object, names(slot_type_list)[i]), # List elements have the names of the slot
#                                  expected_class = slot_type_list[[slot]][1]) # List elements are character vectors of accepted classes
#     if (!class_correct) {
#       # get values needed for the error message
#       is_class <- class(slot(object, slot))
#       should_class <- slot_type_list[[slot]][1]
#       # add error message to list
#       error_list[[as.character(slot)]] <- paste("Entry for ", slot, " has class ", is_class,
#                                                 ". Expected Class(es):", should_class)
#     }
#   }
#   error_list # error list is returned even if its empty
# }

# My tests on check_class
object1 <- new("Rectangles", x = Intervals(c(0, 1)), y = Intervals(c(1, 2)))
check_class(slot(object1,"x"), "Intervals") # returns TRUE as expected
check_class(slot(object1,"x"), "numeric") # returns FALSE as expected

object2 <- new("Rectangles", x = Intervals(rbind(0:1, 1:2)), y = Intervals(rbind(0:1, 1:2)))
check_class(slot(object2,"x"), "Intervals")


### Function get_intervals returns the intervals to make the rectangles in the constructor
# If the entry isnt already an interval it:
# checks if the entries are accepatable returning informative error message if it isn't
# makes the interval

get_intervals <- function(entry){
  # if entry is already an interval that interval is returned.
  if (!check_class(entry, "Intervals")) { # otherwise an interval must be made
    
    # Other than Intervals the only other acceptable classes of entry are:
    # numeric n x 2 matrixes and numeric length 2 vectors. It may not be type z 
    
    ###  Check if entry can be made to a matrix
    # because if Intervals don't reserve a matrix it will try to make one 
    # it must be tested if thats possible
    
    #  But first lists are explicity excluded because
    #  as.matrix(list) returns a list for simplicty this case is excluded
    if (check_class(entry, "list")) {
      stop(paste("The entry for", as.character(bquote(entry)), 
                 "must be an Interval, Matrix or Vector"))
    }
    
    # Check if entry can be made to a matrix
    if (!check_class(as.matrix(entry), "matrix")) { 
      stop("The entry for x must be an Interval, Matrix or Vector") 
    }  
    
    # for converence of testing vectors are made to matrices in this step
    if (!check_class(entry, "matrix")) { 
      # matrix(vector) lists a values in 1 column with many rows, 
      # its needs transposing as Intervals use n x 2 Matrices.
      entry <- t(matrix(entry))
    }  
    
    # check the matrix has two collumns
    if (!(ncol((entry)) == 2)) {
      stop(paste(as.character(bquote(entry)), "has the incorrect number of columns:
                2 are expected as every Interval needs a start and an end value"))
    }
    
    ### check type is nummeric
    if (!is.numeric(entry)) stop(paste("The elements in", as.character(bquote(entry)), 
                                       "must be nummeric"))
    
    ### check start is smaller than end
    # for each interval check if the start is smaller than the end
    start_before_end <- apply(entry, 1, get_start_before_end)
    # If this is true for any row give an error
    if (TRUE %in% start_before_end) stop("Error in ", as.character(bquote(entry)), 
                                          " the first value in each interval 
                                          must be smaller than the second")
    
    # if all checks have been passed then x must be the right type to make an interval
    entry_int <- Intervals(entry)
  } else entry_int <- entry # if x is already an Interval then it is the same as x_int)
  entry_int # return the interval
}

### my Test on get_intervals
matrix1 <- rbind(0:1, 1:2, 2:3)
get_intervals(matrix1) # create interval as exprected

vector <- c(1,2) 
get_intervals(vector) # creates an interval as expected

matrix2 <- rbind(c("a","b"), c("a","b"),c("a","b"))
get_intervals(matrix2) # gives error wrong type as expected

matrix3 <- matrix(c(0,1,1,2,2,3,4,5,6), ncol = 3)
get_intervals(matrix3) # gives error wrong number of dimensions as expected

matrix4 <- rbind(c(3,2), c(1,2))
get_intervals(matrix4) # gives error first values smaller than second as expected


### Constructor
Rectangles <- function(x,y){
  x_int <- get_intervals(x)
  y_int <- get_intervals(y)
  
  new("Rectangles", x = x_int, y = y_int) # return an object of class Rectangles
}

#-------------------------------------------------------------------------------
# CHECK: Rectangles creates a Rectangles S4-object,
# with slots x and y of class Intervals.
test_rect <- Rectangles(Intervals(c(0, 1)), Intervals(c(1, 2)))
is(test_rect, "Rectangles")
is(test_rect@x, "Intervals")
is(test_rect@y, "Intervals")
identical(test_rect@x@.Data, t(matrix(c(0, 1))))
identical(test_rect@y@.Data, t(matrix(c(1, 2))))

#### All TRUE
#-------------------------------------------------------------------------------
# CHECK: constructor works for Intervals & corresponding vector or matrix inputs:
unit_rect <- Rectangles(Intervals(c(0, 1)), Intervals(c(0, 1)))
identical(
  unit_rect,
  Rectangles(c(0, 1), c(0, 1)))
identical(
  Rectangles(Intervals(rbind(0:1, 1:2)), Intervals(rbind(0:1, 1:2))),
  Rectangles(rbind(0:1, 1:2), rbind(0:1, 1:2)))

#### ALL TRUE
#-------------------------------------------------------------------------------
# FAILS:
# The following calls to Rectangles should fail with INFORMATIVE & precise
# error messages:
Rectangles(c(-1, -2), c(0, 1)) 
# returns custom error message that the second number must be smaller than the first
Rectangles(Intervals(c(0, 1), closed = FALSE), Intervals(c(0, 1)))  
# returns custom error message that intervals must be closed
Rectangles(Intervals(c(0, 1), type = "Z"), Intervals(c(0, 1)))
# returns custom error message that intervals may not have type z
Rectangles(Intervals(c(0, 1), type = "Z", closed),
           Intervals(c(0, 1), type = "Z"))
# Returns error message that the interval object is invalid

### My fail test 
Rectangles(Intervals(rbind(0:1, 1:2, 2:3)), Intervals(rbind(0:1, 1:2))) 
# Returns custom error message that the same number of height and width 
