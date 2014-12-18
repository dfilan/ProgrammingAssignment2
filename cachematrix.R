## These functions make a matrix-like object (actually a list) that can cache
## its inverse, and then calculate the inverse if it isn't already cached.

## This makes a 'cache matrix' object, which can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
	m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This takes the 'cache matrix' object, and if it already has an inverse saved,
## returns that inverse. Otherwise, it calculates the inverse of the matrix,
## and returns the result of the calculation.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
	return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
