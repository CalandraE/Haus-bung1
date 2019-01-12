################################## Q2 #######################################
########### a)
`%in%` <- function(x, table) UseMethod("%in%", object = table)

# In this command a new generic function "%in%" is created.
# The object whose class determines which method is called is in this case ...
# table, the second term given to the function. 
# This is unusual as the default is the first term (here x). 
# However here it is nessary in this case to use the second term given ...
# ... to the function as it is important that in commard like:
# "number %in% interval" the method for Intervals objects is used not for ...
# ... numeric vectors (call of first term)

########### b)

# To ensure functionality with normal inputs remains unchanged, ...
# ... the new generic is given a default it can reach back on ...
# ... incase no spesific functions for input class have been defined.
# This in default method is simply the base impliemtation of `%in%`, ...
# ... which itself calls the generic function match

`%in%.default` <- function(x, table) base::`%in%`(x, table)

#### Tests:

source("intervals-in.R")
# make base-version of %in% available for direct comparisons:
`%base_in%` <- base::`%in%`
all.equal(1:5 %in% c(1, 3, 5, 9),
          1:5 %base_in% c(1, 3, 5, 9))
all.equal(c("a", "b", "c") %in% c("a", "b"),
          c("a", "b", "c") %base_in% c("a", "b"))
all.equal(c("a", "b", "c") %in% list(c("a", "b", "c"), "a"),
          c("a", "b", "c") %base_in% list(c("a", "b", "c"), "a"))
##### All true

######################## c) %in% methode for Intervals ########################

#################### Plan


######## Function: `%in%.Intervals`

#### Goal: method for %in% applicable to Intervals Objects that ...
#### checks if numbers lie within intervals


### Inputs: intervals (intervals object)
###         numbers (single number or numeric vector),

### Accepted length of numbers:
## From the tests there are 3 acceptable cases:
# Case 1: 1 number in numbers and mulitple intevals:
### -> test if the number is in each interval
# Case 2: many numbers and one inteval 
### -> test if each number is in the interval
# Case 3: vector numbers has the same length as the number of intervals
### -> test one number per interval

### Not allowed: 
## Case 4: length(numbers) > 1 & #intervals > 1 & length(numbers) != #intervals 
## here its not clear which interval the number should be tested with
## return error message

####### Answer to a question in the tests:
## So if a character like "a" is given to the function it should fail with an 
## informative ...
## ... error message. 
## As the intervals are always between two numbers it doesn't make sense to ...
## ... ask if a character string is within the numeric interval
## Allowing it as an numbers and just returning FALSE would confuse the user.

## Matrices are also not allowed -> return informative error message

## The function should consider if the interval is open or closed
## When determing the number is in the interval or not


### discription:
# Test numbers if is an acceptable input
#### (no need to test the class of the intervals object as the ...
#### ... method wouldn't be called up it wasn't of class intervals)
# determine the case at hand (with, get_case)
# depending on the case: test each interval number combination 
##### (with, test_number_in_interval)
# collect test results in a 'results' vector

### Output: results (logical vector of test results)



################ Function: test_number_in_interval

#### Inputs: number (single number), 
####         interval (Intevals object containing 1 interval)
# i.e. with nrow(interval.Data) == 1

#### Discription:
## Return true if number is less than the upper limit of interval and ...
## ... more than the lower limit

## then for both ends of the interval:
# if intervals is closed and number is equal to at end of interval return true

## return na if number is na

## return false if number isn't an integer but interval has Type Z

## in all other cases return false

### Output: TRUE, FALSE or NA


################ Function: get_case

### Inputs: n_numbers (the length of the numbers vector)
###         n_intervals (number of intervals in the Intervals object)

### Discription: 
# compaire n_number and n_intervals to determine which of the cases ...
# defined above apply.
# If none of the cases apply return an error explaining accepted inputs

### Output: Character sting with the currently applicable class

######## Function: `%in%.Intervals_full`

### Explanation: Intervals_full objects don't automatically call up ...
### ... the method `%in%.Intervals`. So to avoid using the default method ...
### ... a new method for Intervals_full objects is explicitly defined

### Inputs: intervals (intervals object)
###         numbers (single number or numeric vector),

### Discription: exact copy of `%in%.Intervals`

### Output: results (logical vector of test results)

#################### Implimentation

############# Function: test_number_in_interval
test_number_in_interval <- function(number, interval){
  
  ### if number is na then return no, further evaluation nessary
  if (is.na(number)) return(NA)
  
  ### if interval has type Z and number is a decimal it fails automatically
  # Explanation:
  # checkmate::check_integerish returns a character string if the object ...
  # ... is not integerish and TRUE if it is. 
  # Therefore if check_integerish(number) has the class character then number ...
  # ... has a decimal component.
  
  if (interval@type == "Z" && 
      is.character(check_integerish(number))) return(FALSE)
  
  # get the upper and lower limits of interval
  lower_limit <- interval@.Data[,1]
  upper_limit <- interval@.Data[,2]
  
  # test if number is in (lower limt, upper limit)
  if ((number > lower_limit) && (number < upper_limit)) return(TRUE)
  
  # if number is equal to one of the limits and that limit is closed ...
  # ... the test is also passed
  
  if ((number == lower_limit) && (interval@closed[1] == TRUE)) return(TRUE)
  if ((number == upper_limit) && (interval@closed[2] == TRUE)) return(TRUE)
  
  # otherwise the test is failed
  FALSE
}

# my tests: test_number_in_interval
test_number_in_interval(5, Intervals(c(1,10))) # returns TRUE as expected
test_number_in_interval(-5, Intervals(c(1,10))) # returns FAlSE as expected
test_number_in_interval(10, Intervals(c(1,10), closed = c(FALSE,FALSE))) 
# returns FAlSE as expected
test_number_in_interval(10, Intervals(c(1,10), closed = c(FALSE,TRUE))) 
# returns TRUE as expected
test_number_in_interval(NA, Intervals(c(1,10)))
# returns NA as expected

#######  Function: get_case

get_case <- function(n_numbers, n_intervals){

  ## case is intially null
  case <- NULL
  
  ## compaire n_numbers and n_intervals to determin the case
  if ((n_numbers == 1) && (n_intervals > 1)) case <- "case1"
  if ((n_numbers > 1) && (n_intervals == 1)) case <- "case2"
  if (n_numbers == n_intervals) case <- "case3"
  
  ## if case is still null then it is not an accepable combination
  ## -> give an informative error
  
  if (is.null(case)) stop(paste(
    "It is not possibe to compaire this combination of numbers and intevals.",
    " \nOnly the following three cases are acceptable: "
    ,"\n1) numbers has the length 1 ",
    "\n2) intervals contains only 1 interval "
    ,"\n3) numbers has the same length as the number of intervals", 
    sep = ""))
  
  ## return the case
  case
}

### my tests: case 
get_case(2, 2) # case3 as expected
get_case(1, 2) # case1 as expected
get_case(3, 1) # case2 as expected
get_case(3, 2) # error as expected

######## Function: `%in%.Intervals`
`%in%.Intervals` <- function(numbers,intervals){
  ### load package
  library(checkmate)
  
  ### Input tests
  
  # if intervals contains no intervals (ie is an empty intervals object) ...
  # give an error
  if (nrow(intervals@.Data) == 0) stop("intervals cannot be empty")
  
  ### check numbers is a numeric vector (and not matrix)
  assert_numeric(numbers)
  if (is.matrix(numbers)) stop("numbers must be a vector not a matrix")
  
  ### End of tests, start of actual calculation
  
  ## get the size of the inputs
  n_numbers <- length(numbers)
  n_intervals <- nrow(intervals@.Data)
  
  # get the case
  case <- get_case(n_numbers, n_intervals)
  
  # depending on the case a different combination of numbers and intervals ...
  # ... are tested. The result of these tests are returned
  results <- switch(case, "case1" = 
           sapply(c(1:n_intervals),function(x){
             test_number_in_interval(numbers,intervals[x])})
         ,"case2" = 
           sapply(c(1:n_numbers),function(x){
                    test_number_in_interval(numbers[x],intervals[1])})
         ,"case3" = 
           sapply(c(1:n_numbers),function(x){
             test_number_in_interval(numbers[x],intervals[x])}))
  results
}

######## Function: `%in%.Intervals_full`
`%in%.Intervals_full` <- `%in%.Intervals`


####### Answer to question:
ints <- Intervals(cbind(0:1, 1:2))
"a" %in% ints #should fail or return FALSE? explain pros & cons of your decision.

#### fails has it should. 
## Pro: It would be confusing to the user to return fail ...
## ... it doen't make sense ask if a caharcter string lies between two numbers
## con: for single letters like "a" one could get the number of the letter ...
## ... in the alphabet and see if thats in the interval. This potential ...
## ... use case is thereby lost by banning all character inputs.
#### However I think its more important for a function to be easy to ...
#### ... understand than to alow for odd special cases


######## Tests for Q2.c

library(intervals)
#-------------------------------------------------------------------------------
# set up example data:
(ints <- Intervals(cbind(0:1, 1:2)))
(ints_open <- Intervals(cbind(0:1, 1:2), closed = FALSE))
(sets <- Intervals(cbind(c(-5, 0, 5), c(0, 5, Inf)),
                   closed = c(TRUE, FALSE), type = "Z"))
(empty <- Intervals(c(0, 0), closed = FALSE))

(na <- Intervals(c(0, NA), closed = TRUE))
#-------------------------------------------------------------------------------
#CHECK: basic functionality:
all.equal(1 %in% ints[1], TRUE) # is TRUE
all.equal(1 %in% ints_open[1], FALSE) # is TRUE
all.equal(NA %in% ints[1], NA) # is TRUE
all.equal(NA %in% na, NA) # is TRUE
all.equal(0 %in% na, TRUE) # is TRUE
all.equal(0 %in% empty, FALSE) # is TRUE
#-------------------------------------------------------------------------------
#CHECK: vector inputs:
all.equal(c(-1, .5, 1, 0) %in% ints[1],
          c(FALSE, TRUE, TRUE, TRUE)) # is TRUE
all.equal(c(-1, .5, 1, 0) %in% ints_open[1],
          c(FALSE, TRUE, FALSE, FALSE)) # is TRUE
all(c(.5, 1.1) %in% ints) # is TRUE
all.equal(.5 %in% ints,
          c(TRUE, FALSE)) # is TRUE
all.equal(5 %in% sets,
          c(FALSE, FALSE, TRUE)) # is TRUE
all.equal(0.5 %in% sets,
          c(FALSE, FALSE, FALSE)) # is TRUE
#-------------------------------------------------------------------------------
#CHECK: dealing with unsuitable/weird inputs:

"a" %in% ints #should fail or return FALSE? explain pros & cons of your decision.
#### fails has it should. 
## Pro: It would be confusing to the user to return fail ...
## ... it doen't make sense ask if a caharcter string lies between two numbers
## con: for single letters like "a" one could get the number of the letter ...
## ... in the alphabet and see if thats in the interval. This potential ...
## ... use case is thereby lost by banning all character inputs.
#### However I think its more important for a function to be easy to ...
#### ... understand than to alow for odd special cases

# The calls below should fail with informative error messages:
c(.5, 1.1, 2) %in% ints #should fail # does fail, with expected error message
matrix(0, 2, 2) %in% ints #should fail # does fail, with expected error message
list(0, 2, 2) %in% ints #should fail # does fail, with expected error message
#-------------------------------------------------------------------------------
#CHECK: deal with open and closed intervals: # all true
(ints_all <- c(ints, ints_open))
all.equal(1 %in% ints_all,
          c(TRUE, TRUE, FALSE, FALSE))
all.equal(c(0, 2, 0, 2) %in% ints_all,
          c(TRUE, TRUE, FALSE, FALSE))
all.equal(c(-1, 1, 0.5, 2) %in% ints_all,
          c(FALSE, TRUE, TRUE, FALSE))

