## Below are a set of functions that can be used to cache matrix inverse
## calculations, to avoid having to recompute the inverse several times for the
## same matrix. Users should first call makeCacheMatrix in order to get a
## special wrapper around the matrix that includes convenience functions to
## store and access the cached inverse, and then call cacheSolve on this object
## to compute the inverse. The cached inverse will be used if available.

## The makeCacheMatrix takes a single argument which is the matrix to be wrapped,
## and returns a list object that implements functions to:
##
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
##
## Setting the value of the matrix resets the inverse cache.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve takes one required argument which is a list such as that
## returned by makeCacheMatrix, and returns the inverse of the matrix. The
## cached value is used if available, and if not, the cache is set so that
## future calls return the cached inverse (unless the matrix is modified).
## Additional arguments are passed to the solve function.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached matrix inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
