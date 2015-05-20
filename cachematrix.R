## This file is part of the second programming assignment for the
## `R Programming` course from Johns Hopkins University, which is
## part of its Data Science Specialization available at Coursera.
## See README.md for details.

## Creates a special type of "matrix", that we call cacheMatrix,
## which is a list containing four functions:
## - setMatrix:  set the matrix
## - getMatrix:  get the matrix
## - setInverse: set the value of the inverse matrix
## - getInverse: get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    setMatrix <- function(newMatrix) {
        x <<- newMatrix
        inverse <<- NULL
    }
    
    getMatrix <- function() {
        x
    }
    
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    getInverse <- function() {
        inverse
    }
    
    list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Receives a cacheMatrix and, assuming that the matrix supplied is
## always invertible, returns its inverse using its cached value,
## if available, or computes and saves it, otherwise. Any extra
## arguments can be consulted at solve() R function's documentation
## that will be used to compute the inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$getMatrix()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
