## Put comments here that give an overall description of what your
## functions do

## The functions below uses cache to help with potential performance problems
## when using matrix inversion on the same matrix repetedly. They store the
## inverted  matrix in a cache the first time it is calculated for a matrix and
## then retrieved from the cache if inversion is called again for the same matrix


## Write a short comment describing this function

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It provides helper functions to get/set the original matrix and inverse matrix 
##
## Arguments:
##      x is the original matrix
##
## Return:
##      list with get/set functions to manipiulate the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## x is the original matrix
    ## m is the inverse matrix of x
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
## Arguments:
##      x is the special "matrix" object returned by makeCacheMatrix
##      ... are more argments as needed
##
## Return: Inverse of of original matrix x


cacheSolve <- function(x, ...) {
        
    ## x is the special "matrix" object returned by makeCacheMatrix
    ## m is the inverse matrix of the original matrix stored in x
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}

## Sample Test:
## > x <- matrix(1:4,2,2)
## > x
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m <- makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 