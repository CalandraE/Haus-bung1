

######################## Methode: Size #############################

##### Plan for size

## Formal Arguments: 1 Rectanges object

# Use setMethod to extend the existing generic size to Rectanges

## Calculate the size for each rectanges in the Rectanges object
####### Can use size on intervals to get the distance between the two
########### Extract one interval of width and the corresponding one of height
########### Get there size
########### Multiply together
########### Store in a vector
## Sum together

## Return: numeric value of sum of size


#### Examine Intervals more closely
matrix1 <- rbind(0:1, 1:2, 2:3)
intervals1 <- get_intervals(matrix1)
intervals1
size(intervals1)

matrix2 <- rbind(c(0,1), c(1,20), c(2,300))
intervals2 <- get_intervals(matrix2)
size(intervals2)


######## Code for Size
# Function to find the size of rectanges
size_Rectangles <- function(rectangles){
  # return error unless "Rectanges" object is whats is given
  #if (!check_class(rectangles, "Rectangles")) stop(
  #  "The function can only be appled to objects of the class Rectangles")
  
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

get_distance <- function(start, end){
  # check that start and end are nummeric
  assert_numeric(start)
  assert_numeric(end)
  
  # return difference between start and finish 
  distance <- abs(end - start)
  
  distance
}

# my test

Rectangles1 <- Rectangles(Intervals(rbind(0:1, 1:2)), Intervals(rbind(0:1, 1:2)))
size_Rectangles(Rectangles1) # 1,1 as expected (1*1 + 1*1)

Rectangles2 <- Rectangles(Intervals(rbind(c(0,10), c(0,5))), Intervals(rbind(c(0,10), c(0,5))))
size_Rectangles(Rectangles2) # 100, 25 as expected (10*10 + 5*5)


# set size_Rectangles as the Methode Size for Rectangles

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
rect
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
