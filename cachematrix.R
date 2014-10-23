## Put comments here that give an overall description of what your
## functions do

# This returns a list of functions that can cache the inverse of a matrix
# set(X) :: sets the matrix
# get()  :: returns the matrix
# setInverse(i) :: sets the inverse value
# getInverse()  :: gets the inverse value
# this assumes the parameter X is invertable
makeCacheMatrix <- function(x = matrix()) {
    # initialize the cache to null
    inverseCache <- NULL
    set <- function(y) {
        x <<- y
        # don't forget to clear the cache if the matrix changes
        inverseCache <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseCache <<- inverse
    getInverse <- function() inverseCache
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# This functions takes the result of the above function 
# and computes the inverse if it hasn't been already
# Usage::
# myInverse = cacheSolve(makeCacheMatrix(myMatrix))
cacheSolve <- function(cacheMatrix, ...) {
    
    # has the inverse already been computed?
    inverse <- cacheMatrix$getInverse()
    if(!is.null(inverse)) {
        # yes! return the cached inverse
        message("Getting Cached Data")
        return(inverse)
    }
    # not yet. compute the inverse ...
    data <- cacheMatrix$get()
    inverse <- solve(data, ...)
    # save the inverse into the cache
    cacheMatrix$setInverse(inverse)
    inverse
}

#X = matrix(rexp(16, rate=.1), ncol=4)
#X
#cacheMatrix <- makeCacheMatrix(X)
#cacheSolve(cacheMatrix)
#cacheSolve(cacheMatrix)


