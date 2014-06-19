## Author: ssenpa
## Date: 06/19/2014

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. The following two functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ivx <- NULL
  set <- function(y) {
    x <<- y
    ivx <<- NULL
  }
  get <- function() x
  setivx <- function(ivrs) ivx <<- ivrs
  getivx <- function() ivx
  list(set = set, get = get,
       setivx = setivx,
       getivx = getivx)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ivx <- x$getivx()
  
## If the inverse is already calculated, return it  
  if(!is.null(ivx)) {
    return(ivx)
  }
  
## If the inverse is not yet calculated, calculate it and catch it   
  data <- x$get()
  ivx <- solve(data, ...)
  x$setivx(ivx)
  ivx
}



