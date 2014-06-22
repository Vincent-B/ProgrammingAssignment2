# Matrix inversion is usually a costly computation. These functions will allow
# the caching of the inverse of a matrix.
# Note: These functions will assume that the matrix suppplied is invertible.


# makeCacheMatrix creates a list of the matrix that can cache its inverse, which
# can be accessed by makeCacheMatrix
# This list contains 4 functions:
    # set <- sets (copies) the values of the matrix
    # get <- returns the values of the matrix
    # setinverse <- sets (computes) the inverse of the matrix
    # getinverse <- returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve checks if the inverse of a matrix returned by makeCacheMatrix has
# already been calculated and will retrieve the inverse from cache if possible.
# Otherwise it will compute the inverse and cache it using makeCacheMatrix.

cacheSolve <- function(x, ...) {
    # Returns a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
