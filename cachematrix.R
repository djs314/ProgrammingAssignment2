## These functions cache the inverse of a matrix, so as to only
## compute it once and not repeatedly, as matrix inversion is
## computationally inefficient

## makeCacheMatrix returns a list of functions that sets the matrix,
## gets the matrix, sets the matrix inverse, and gets the matrix
## inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse from the cache if it has already
## been calculated, and calculates it and sets it in the cache if
## it has not

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}