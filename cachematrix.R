## Put comments here that give an overall description of what your
## functions do

## version:	1.0
## author:	jin.jiangli@mayo.edu

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
##
## @param x A matrix can be inversed
##
## @return A list containing a function to:
##     1 set the value of the matrix
##     2 get the value of the matrix
##     3 set the value of the inverse of the matrix
##     4 get the value of the inverse of the matrix
##

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## this function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
##
## @param x A special matrix created with makeCacheMatrix
##
## @return  The inverse of the matrix x
##

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
