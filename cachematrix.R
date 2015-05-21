## The makeCacheMatrix function is a factory function that returns an object which is a wrap around a matrix and its inverse value
## The cacheSolve uses the object above to update the inverse value if it is missing and retrieve it
## constructor: takes one argument, the initial matrix, by default creates with an empty matrix

## The set(newVal) method sets the new matrix
##


## The factory that returns the object, a simple storage container, with the following API:

# Constructor - initial matrix, defaults to matrix()

# set(newVal) - setter for the matrix value
# get() - getter for the matrix value
# setInv(newInv) - new inverse value
# getInv() - getter for the inverse value
makeCacheMatrix <- function(x = matrix()) {

# to find out if the matrix is invertable, can use:  class(try(solve(x), silent=T))=="matrix"
    recalc <- function() {
        invVal <<- solve(x)
    }

    invVal <- NULL # Set inverted value to NULL
# setter for the matrix, invalidates the cache
    set <- function(newVal) {
        x <<- newVal
        invVal <<- NULL # invalidate the cache
    }

# simple getter for the matrix
    get <- function() x
# simple getter for the inverse value
    getInv <- function() invVal
# simple setter for the inverse value, just stores it
    setInv <- function(newInv) invVal <<- newInv

# return the list of the API functions
    list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## Cache resolver for the makeCacheMatrix: takes one parameter which is the object returned by makeCacheMatrix and returns the inverse value if it is
## set, otherwise computes, stores and returns it

cacheSolve <- function(mxWrap) {
        inv <- mxWrap$getInv() # obtain the inverse value whatever it is
        if(!is.null(inv)) { # if it is non-nul (valid) - recalculate it
            message("Inverse value cache hit")
            return(inv)
        }

        message("Cache miss - recalculating the inverse")

        val <- mxWrap$get() # get the matrix
        inv <- solve(val) # invert it
        mxWrap$setInv(inv) # store it
        inv # return it
}

