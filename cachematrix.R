## Coursera R Programming assignment 2
## This assignment involves defining two functions to cache and calculate the inverse of a square matrix

## The function makeCacheMatrix places a matrix in the cache and has associated functions get, set, getinv, setinv to
## get and set the matrix as well as its inverse
makeCacheMatrix <- function(x = matrix()) {
    matrixinv <- NULL

    # set the matrix entries
    set <- function(y) {
        x <<- y
        matrixinv <<- NULL
    }
    # return the matrix
    get <- function() x
    # set the inverse matrix
    setinv <- function(invm) matrixinv <<- invm
    # return the inverse matrix
    getinv <- function() matrixinv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the function cacheSolve will return the inverse of the matrix from cache if it is already there otherwise it will
## calculate the inverse of the given matrix and store it in cache
cacheSolve <- function(x, ...) {
    # get the inverse of the matrix from cache
    matrixinv <- x$getinv()
    
    # if the matrix inverse has already been calculated then return it
    if(!is.null(matrixinv)) {
        message("returning cached matrix inverse")
        return(matrixinv)
    }
    
    # if the matrix inverse has not yet been calculate then calculate it and return it
    data <- x$get()
    matrixinv <- solve(data)
    x$setinv(matrixinv)
    
    # return the inverse of the matrix
    matrixinv
}
