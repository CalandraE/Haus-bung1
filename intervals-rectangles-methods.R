library(checkmate)
# install.packages("intervals")
library(intervals)


######################## Methode: Size #############################



### Looks at code for the show method for intervals objects
getMethod("show", "Intervals_virtual")

#### Code for the show method for Intervals
# function(object) 
# {
#   cat("Object of class ", class(object), "\n", nrow(object), 
#       " interval", ifelse(nrow(object) == 1, "", "s"), " over ", 
#       type(object), ":\n", sep = "")
#   ints <- as(object, "character")
#   if (!is.null(rownames(object))) {
#     fmt <- sprintf("%%%is", max(nchar(rownames(object))))
#     ints <- paste(sprintf(fmt, rownames(object)), ints)
#   }
#   cat(ints, sep = "\n")
# }
### With examples
Intervals1 <- Intervals(rbind(c(1,2)))
show(Intervals1)

Intervals2 <- Intervals(rbind(c(1,2),c(3,4),c(5,9)))
show(Intervals2)

Intervals3 <- Intervals(rbind(c(1,3),c(3,5),c(5,9)))
rownames(Intervals3) <- c("Int1","Int2","Int3")
show(Intervals3)

####### aimed for represetaion with show for Rechtangels:

## Object of class Rectangles with 3 rectangles:
## [0, 1] x [0, 2]
## [-1, 2] x [1, 2]
## [0, 1] x [-Inf, Inf]


### Plan of modification needed to show(Intervals) for Rectangles object
## command cat...: 
# replace nrow(object) with nrow(object@x@.Data) (in both places)
# replace " interval" with " rectangles"

## command ints<-:
## command if...:
# turn into seperate function get_ints_with_names

# replace ints with ints_x
# make a corresponding ints_y
# fill both with the function get_ints_with_names

## comand cat...:
# write function that prints withd interval " x "  y interval
# call for the number of intervals printing each in a row


### Funciton for Checking an object is a valid intervals object with 1 interval
test_single_interval <- function(interval, single = TRUE){
  name_object <- as.character(substitute(interval))
  
  ### Check object is an Intervals object 
  if (!check_class(interval,"Intervals")) stop(
    paste(name_object,"must be an Intervals object", sep = " "))
  
  ### Check it a valid Intervals objects
  if (!validObject(interval)) # no stop function required
    # because if it is not a valid Intervals object then pre-implemented error
    # messages that fits the type of objects thats given is returned
    
    ### Only test if its a single Interval if single is set to TRUE
    if (single == TRUE) {
      ### Check it contains a single Interval
      if (!identical(nrow(interval@.Data), 1)) stop(
        paste(name_object,"may only contain one Interval", sep = " "))
    }  
  
  
  ### return nothing (function only does something if there is a problem)
}

##### my test for is_single_interval
# pass test
test_single_interval(Intervals1) #  returns nothing as intended
# fail tests
vec <- c(2,3)
test_single_interval(vec) # returns error: Not of class "Intervals"

fake_int_M <- rbind(c(1,2),c(3,4))
class(fake_int_M) <- "Intervals"
test_single_interval(fake_int_M) # returns error message built into Intervals

fake_int_V <- c(1,2)
class(fake_int_V) <- "Intervals"
test_single_interval(fake_int_V) # returns different error message


### Function to return intervals as (named) character vector
get_ints_with_names <- function(object,slot){
  checkmate::assert_character(slot) # check that slot is given as a character
  if (!(length(slot)) == 1) stop("slot must have length 1") # do not allow vectors
  # Modified from show(Intervals):
  ints <- as(slot(object,slot), "character")
  # function that gets the row names if there are any
  if (!is.null(rownames(slot(object,slot)))) {
    fmt <- sprintf("%%%is", max(nchar(rownames(slot(object,slot)))))
    ints <- paste(sprintf(fmt, rownames(slot(object,slot))), ints)
  }
  ints
}

### Funciton that pastes the interval of x and y for each Rectangle in a row
# in the desired form
get_row_text <- function(x_single_int, y_single_int){
  
  # If the Itervals is not empty test that its a single Interval
  # Does nothing if no test is nessary
  # if (!identical(x_single_int,Intervals())) test_single_interval(x_single_int)
  # if (!identical(y_single_int,Intervals())) test_single_interval(y_single_int)
  
  # check that the intervals are give as characters
  checkmate::assert_character(x_single_int)
  checkmate::assert_character(y_single_int)
  # make sure only one interval is given at a time
  if (!(length(x_single_int)) == 1) stop("x_single_int must have length 1")
  if (!(length(y_single_int)) == 1) stop("y_single_int must have length 1")
  
  # # Special case: Paste for empty intervals
  # if (identical(x_single_int,Intervals()) && 
  #     identical(y_single_int,Intervals())) return(paste("[ , ] x [ , ]"))
  
  # return string containing the text to print for each rectangle
  # eg. [0, 1] x [0, 2]
  paste(x_single_int, "x", y_single_int, sep = " ")
}





### My test
# get some named intervals
Intervals4 <- Intervals(rbind(c(1,2),c(3,4),c(5,9)))
rownames(Intervals4) <- c("width1","width2","width3")
Intervals5 <- Intervals(rbind(c(11,12),c(13,14),c(15,19)))
rownames(Intervals5) <- c("height1","height2","height3")
# make a rectangle
rectangles1 <- Rectangles(Intervals4,Intervals5)
# test get_ints_with_names -> works as expected
ints_x1 <- get_ints_with_names(rectangles1,"x") 
ints_x1 # returns intervals with names
ints_y1 <- get_ints_with_names(rectangles1,"y") 
ints_y1 # returns intervals with names
# tests get_row_text -> works as expected

get_row_text(ints_x1[1],ints_y1[1]) 
# prints first rechtangle's intervals in the expected from

### Actual show funktion for rectangles
show_Rectangles <- function(rectangles){
  # return error unless "Rectanges" object is whats is given
  if (!check_class(rectangles, "Rectangles")) stop(
    "The function can only be appled to objects of the class Rectangles")
  
  # Check a valid Rectangles object was given
  if (!validObject(rectangles)) stop(
    "This function can only be applied to valid Retangles objects")
  
  ### Special case: Show for empty Rectangles Objects
  # ie when both intervals are empty
  if (identical(rectangles@x,Intervals()) && 
      identical(rectangles@y,Intervals())) return(cat("[ , ] x [ , ]"))
  
  # get character vectors of x and y intervals
  ints_x <- get_ints_with_names(rectangles,"x")
  ints_y <- get_ints_with_names(rectangles,"y")
  
  # get number of rectangels
  n <- length(ints_x) 
  # Note: Rectangels are defined so that length(ints_x) = length(ints_y)
  
  # get a vector containing the rows texts for each rectangle
  rows <- sapply(1:n, function(i) get_row_text(ints_x[i],ints_y[i]))
  
  
  # first row of text
  cat("Object of class ", class(rectangles), " with ", nrow(rectangles@x@.Data), 
      " rectangle", ifelse(nrow(rectangles@x@.Data) == 1, "", "s"), 
      ":\n", sep = "")
  # print dimensions of all rectanges in a column 
  cat(rows, sep = "\n")
}

# my tests ### doesnt work any more
show_Rectangles(rectangles1) # works with names
rectangles2 <- Rectangles(rbind(c(1,2),c(2,3)), rbind(c(-1,2),c(2,4)))
show_Rectangles(rectangles2) # works without names

# set show_Rectangles as the Methode show for Rectangles

setMethod("show", signature(object = "Rectangles"), function(object) {
  show_Rectangles(object)
})

# check rectanges are shown in the desiered form
rectangles1
rectangles2
# works for both

######################## Methode: Size #############################

##### Plan for size

## Formal Arguments: 1 Rectangles object

# Use setMethod to extend the existing generic size to Rectangles

## Calculate the size for each Rectangles in the Rectangles object
####### Can use size on intervals to get the distance between the two
########### Extract one interval of width and the corresponding one of height
########### Get there distance
########### Multiply together
########### Store in a vector
## return vector

## Return: vector with sizes

######## Code for Size

# Function for finding the breadth of a single interval
get_distance <- function(start, end){
  # check that start and end are nummeric
  assert_numeric(start)
  assert_numeric(end)
  
  # return difference between start and finish 
  distance <- abs(end - start)
  
  distance
}

# Function to find the size of rectanges
size_Rectangles <- function(rectangles){
  # return error unless "Rectanges" object is whats is given
  if (!check_class(rectangles, "Rectangles")) stop(
   "The function can only be appled to objects of the class Rectangles")
  
  # Check a valid Rectangles object was given 
  if (!validObject(rectangles)) stop(
  "This function can only be applied to valid Retangles objects")
  
  # Per defintion of the class Rectanges the intervals with height and width must
  # be the same size. So the number of rows in x are the number of individual triangels
  # n <- nrow(slot(slot(rectangles, "x"),".Data"))
  n <- nrow(rectangles@x@.Data)
  
  # make a vector to hold the sizes of each rechtanges
  size_each <- rep(NA, length = n)
  
  # fill vector with sizes
  for (i in 1:n) {
    # if any of the intevals of the rectangle are NA return NA
    if (NA %in% c(rectangles@x@.Data[i,],rectangles@y@.Data[i,])) NA
    else {# else calculate size
    # get the size of the sides of each Rectangle
    width <- get_distance(rectangles@x@.Data[i,1], rectangles@x@.Data[i,2])  
    height <- get_distance(rectangles@y@.Data[i,1], rectangles@y@.Data[i,2])  
    # calculate and record size
    size_each[i] <- width * height
    }
  }
  
  # return vector of sizes
  as.numeric(size_each) # this converts NAs from NA logical to NA real
}



# my test

Rectangles1 <- Rectangles(Intervals(rbind(0:1, 1:2)), 
                          Intervals(rbind(0:1, 1:2)))
size_Rectangles(Rectangles1) # 1,1 as expected (1*1 + 1*1)

Rectangles2 <- Rectangles(Intervals(rbind(c(0,10), c(0,5))), 
                          Intervals(rbind(c(0,10), c(0,5))))
size_Rectangles(Rectangles2) # 100, 25 as expected (10*10 + 5*5)


# set size_Rectangles as the Methode Size for Rectangles

# Unlike for show, setMethod only works if a new Generic is defined
setGeneric("size", function(object) {
  standardGeneric("size")
})

setMethod("size", signature(object = "Rectangles"), function(object) {
  size_Rectangles(object)
})

# showMethods("size")

#-------------------------------------------------------------------------------
rect <- Rectangles(
  x = rbind(c(0, 1), c(-1, 2), c(0, 1)),
  y = rbind(c(0, 2), c(1, 2), c(-Inf, Inf)))
rect_na <- Rectangles(c(0, NA), c(1, 2))
rect_line <- Rectangles(c(0, 0), c(1, 2))
#-------------------------------------------------------------------------------
# CHECK: show-method works as expected:
rect # works as desired
## Object of class Rectangles with 3 rectangles:
## [0, 1] x [0, 2]
## [-1, 2] x [1, 2]
## [0, 1] x [-Inf, Inf]
#-------------------------------------------------------------------------------
# CHECK: size-method works as expected:
all.equal(size(rect), c(2, 3, Inf)) # works as desired
## [1] TRUE
all.equal(size(rect_na), NA_real_) # works as desired
## [1] TRUE
all.equal(size(rect_line), 0) # works as desired

# my test
rect_na_num <- Rectangles(rbind(c(0, NA), c(1, 2)), rbind(c(0, 5), c(1, 2)))
all.equal(size(rect_na_num), c(NA_real_,1)) # works as expected

