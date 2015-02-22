## This file contains multiple functions that allow one to:
## - Calculate an inverse matrix
## - Cache a inverse matrix
## - Set / Get the cached inverse matrix
## - Create a cache to store the matrix


## This function creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the inverse matrix
## 2. get the value of the inverse matrix
## 3. set the value of the original matrix
## 4. get the value of the original matrix
makeCacheMatrix <- function(m = matrix()) {
	# iM is the inverse matrix
	# m is the original matrix
	
	iM <- NULL
	
	# Set original matrix function
	set <- function(y) {
		m <<- y
		iM <<- NULL
	}

	# Get original matrix function
	get <- function() m

	# Set the inverse matrix function
	setInverse <- function(inverse) {
		iM <<- inverse
	}

	# Get the inverse matrix function
	getInverse <- function() iM

	# List the functions to the environment
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse matrix and stores the value in the cache matrix.
## If the value of the matrix is already cached, it returns the value rather than re-calculating it.
cacheSolve <- function(x, ...) {
    # Grab the cached inverse matrix of x (may return null)
    iM <- x$getInverse()

    # Check if the cache is null
    if(!is.null(iM)) {
    	message("getting cached data")
    	return(iM)
    }

    # Call get on x, this creates cache for inverse matrix
    data <- x$get()
    # Calls set on x, this caches the original matrix
    x$set(x)
    # Calls solve on x, passes ... from original call, this inverses the input matrix
    iM <- solve(x, ...)
    # Calls setInverse to store inverse matrix in cache
    x$setInverse(iM)

    ## Returns inverse matrix
    iM
}