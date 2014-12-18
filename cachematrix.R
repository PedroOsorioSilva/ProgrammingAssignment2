## makeCacheMatrix and cacheSolve together will
## cache the inverse of a matrix for future use, in the event 
## there is a need to recalculate it and the matrix has not changed.

## makeCacheMatrix: creates a "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(   set = set, 
		    get = get,
		    setinverse = setinverse,
		    getinverse = getinverse)
}

## cacheSolve: computes the inverse of the special "matrix"
## created by makeCacheMatrix. If the inverse has already
## been calculated and the matrix has not changed,
## cacheSolve retrieves the result from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached invested matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
