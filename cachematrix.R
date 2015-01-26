## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This assignment assumes the matrix supplied is always invertible
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the 
## cache via the setsolution function

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
