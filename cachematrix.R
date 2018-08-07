## First function makeCacheMatrix creates a "matrix" which is a list
## containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get, 
          setinverse =setinverse, getinverse = getinverse)
}

## This function computes the inverse of the "matrix" returned by
## makeCacheMatrix. If the inverse has been computed, it gets the
## inverse from cache and skips the computation.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("Getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}