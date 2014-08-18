## Author: Piyush Sharma
## Date: 08/18/2014
## Time: 10:52

## This file has two R functions that together can be used to create a 
## matrix and cache the inverse of that matrix. These functions are useful 
## because matrix inversion is usually a costly computation and by caching 
## the inverse, we can just look up for the inverse in the cache and avoid 
## recomputing the inverse. 
## We use the solve function to compute the inverse of a square matrix. 
## NOTE => We assume that the input matrix is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that 
##                  can cache its inverse. 
## Returns a list containing functions to:
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse of the given matrix
##      get the value of the inverse of the given matrix
## Example Usage => makeCacheMatrix(matrix(1:4, 2, 2))
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(matrix) {
        x <<- matrix
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(matrixInverse) matrixInverse <<- matrixInverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("Getting cached matrix inverse")
        return(matrixInverse)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}
