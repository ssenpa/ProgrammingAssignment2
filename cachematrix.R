## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ivx <- x$getivx()
  if(!is.null(ivx)) {
    message("getting cached data")
    return(ivx)
  }
  data <- x$get()
  ivx <- solve(data, ...)
  x$setivx(ivx)
  ivx
}



