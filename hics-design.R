# Computes 2D-subspace contrasts for (all, or at most <max_spaces>) pairs
# of numeric columns of <data>.
#
# Inputs:
# data: a matrix or a data.frame with at least 2 numeric columns

# spaces: (optional) 2 x <no. of subspaces> numeric matrix giving the pairs
# of columns that define the subspaces for which the contrasts should be
# computed. If not supplied, all possible subspaces (up to at most
# <max_spaces>, randomly selected) are used.

# deviation: which distance measure for empirical cumulative distribution
# functions to use. Either "ks" for 1 - p-value of the KS-Test
# (default, see stats::ks.test), "cvm" for 1 - p-Value of
# the Cramer-von Mises-Test (see goftest::cvm.test), or "tw"
# for 1 - p-value of Welch's t-test (see stats::t.test) or a user-defined
# function with an argument <conditional> which is a numeric
# vector containing the observations in the conditioning slice and an argument
# <marginal> which is a numeric vector containing all observations.
# The function should return the dissimilarity of the two distributions, i.e.,
# higher values for subspaces with stronger dependencies.

# slice: proportion of data to condition on, defaults to 0.2 (= square root of alpha).

# draws: how often to repeat the conditioning on random "slices" of the
# conditioning variable, defaults to 100 (= M)

# max_spaces: how many 2D subspaces to investigate at most if
# \code{spaces} is not supplied. Defaults to 4950, i.e. choose(100, 2),
# the number of 2D subspaces for a 100D dataset.

# seed: (optional) random generator seed
#
# Output:
# a \code{data.frame} with the column numbers of the investigated subspaces
# in the first two columns (named "dim_1", "dim_2"") and the contrast values
# in the third column (named "deviation"),
# rows sorted so the highest contrast subspaces are in the first rows.

###############################################################################

### Goal: find subspaces with the highest contrast ie which are most correlated

### Idea:

### Step 1: get the matrix with all subspaces to explore.

#### Step 2: For each supbset
## Apply the following <draws> times and then average
## (1) randomly (with seed) choose which variable will be condtioned upon
## (2) randomly (with seed) Get a subset of the data within a certain interval
#### That contains <slice> of the data of the variable condition upon
## (3) calculate the devation between the condtional and marginal distribution

### Step x: sort output max so that the heighest subspace matrix is at the top

###################### Stucture Plan:

# Function 0: get_contrasts
### This is meant to supliment the discription given the quesion...
### ... with a plan of how the outer most function is structured

### Inputs: data, spaces, deviation, slice, draws, max_spaces, seed
## NOTE: these inputs are explained above (their discription was ...
## ... given in the question). I will not explain these again.
## I will also only explain other Inputs used in the subfunctions once.
## If the same name is used again as Inputs in other functions ...
## ... it can be assumed that the same thing is being refered to.

### Discription:
# Create an empty 3 x <number of subspaces> matrix ("results") ...
# ... with collumn names ("dim1", "dim2", "deviation")

# Get a matrix with all the subspace that will be explored (get_subspaces)

#### For each row in get_subspace
## If <spaces> isn't supplied: ...
####... Get the relevant subset of data for the subspace combination (get_subspace_data)
## Calculate the contrasts for the subspace in question (calculate_contrast)
## Write the contrast value together with the columns its the subspace ...
### into a row in "results"
## reorder "results" so that it is sorted by size of contrasts (desening)

### Output: "results" (3 x <number of subspaces> matrix)

############################

# Function 1: get_subspaces

### Inputs: dimensions (number of columns in data), max_spaces, seed, spaces

### Discription:
## If spaces aren't suplied then find all subspaces (F1.1 get_all_subspaces)
## randomly (with seed) select up to max <max_spaces> of the subspaces ...
## ...that will actually be used (F1.2 get_use_subspaces)

### Outputs: numeric matrix of pairs

###############

# Function 1.1: get_all_subspaces

### Inputs: dimensions

### Discription:
## Create a vector with numbers 1:<dimensions> ("dimensions_vector")
## get all combinations of the dimensions: use t(combn(dimensions_vector, 2))

### Output: matrix with all the combinations

###############

# Function 1.2: get_use_subpaces

### Inputs: all_subspaces (matrix with all unique subspace combinations, ...
## ... from get_all_subspaces), max_spaces, seed

### Discription:
## From a vector containing the row numbers of each subspace combination ...
## ... (c(1:nrows(all_subspaces)))
## Randomly (with seed) Draw with out replacing <max_spaces> numbers out of the vector
## get a subset of all_subspaces containing only those rows whose number was drawn

### Output: matrix containing this subset of all_subspaces 


############################

### Function 2: get_subspace_data

### Inputs: data, index_space (index number of the space currently being tested),
### ... subspace_combinations (numeric matrix of dimension pairs ...
### ... that will be tested, from get_subspace)

### Discription:
## use the index to get the relevant row out of the matrix of pairs
## get only those rows of data which belonging to the current pair

### Outputs: data.frame with the relevant subset of data 

############################

# Function 3: calculate_contrast

### Inputs: subspace_data (subset of the data containing only the relevant ... 
# ... 2 columns, from get_subspace_data), slice, deviation, seed, draws, index_space

### Discription:
###### repeate <draws> times:
## randomly (with seed) get conditional variable (F3.1 get_which_conditional)
## get "independant_marginal" the column of <subspace_data> ... 
## ... i.e. the column not selected by get_which_conditional
## get the a slice of the dataset containing <slice> of the data ordered by the ... 
## ...conditional variable (selected by get_which_conditional) (F3.2 get_data_slice)
## Apply the test(s) in <deviation> ...
## ... Get 1-pvalue(s) of the test result (average if there are muliple ones)...
## ...(F3.3 get_each_deviation)

### Output: numeric value of the average results of each run though

###############

# Function 3.1: get_which_conditional
### Inputs: seed, index_space, ...
### ... index_draw (index number of the draw currently being carried out)
## NOTE: the indexs are needed to make the seed different for each itteration

### Discription
# Set the seed using a combinaiton of <seed> and the two indexs ...
# ... (by subset and by draw)
# randomly select 1 or 2 with equal probality

### Outputs: 1 or 2

###############

# Function 3.2: get_data_slice
### Inputs: subspace_data, conditional_index (the number of the collumn ...
### ... which is conditioned on, from get_which_conditional)
### seed, slice, index_space, index_draw

### Discription:
## Order <subspace_data> by <conditional_index> in assending order
## get "slice_width"  which is the number of rows the slice should contain ...
## ... (<slice>*nrows(<subspace_data>)). Round up so that a <slice> fraction ...
## of rows are certianly contained data slice
## randomly (with seed) select a "starting_point" for the slice ...
## (between 1 and N (total rows in <subspace_data>) - "slice_width")
## get "subspace_slice": a subset <subspace_data> to only containing
## the next "slice_width" rows starting from "starting_point"

### Output: the independant column of "subspace_slice" (ie not <conditional_index>)

###############

# Function 3.3: get_each_deviation
### Inputs: independant_marginal (vector containig all the data points for ...
### for the independant variable, From the parent function F3: calculate_contrast),
### independant_conditional (vector containing the datapoints for the ...
### ... independant variable contained the data slice,from F3.2: get_data_slice),
### deviation

### Discription:
## create a vector "devations" the length of <deviation>
### For each entry in <deviation>
## Apply <deviation>[index] to independant_marginal and independant_conditional
## Extract the p values of the test
## get 1-said p value
## record value in "devations"[index]
## Average the results in "devations"

### Note: if <deviation> has the length 1 or a custom function is used it should work fine
## as length(Any_Function_name) has the length 1

### Output: Average result (numeric value)
