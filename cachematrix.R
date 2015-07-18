## These functions are used to compute and cache
## a matrix and its inverse.

## This function creates a special "matrix", that
## is capable of caching its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL
  set <- function(x) {
    mtx <<- x
    inv <<- NULL
  }
  get <- function() mtx
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This method uses "makeCacheMatrix". if
## the inverse of matrix is cached, returns the
## cached value, else computes the inverse, caches 
## the result and returns the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
  } else {
    mtx <- x$get()
    inv <- solve(mtx, ...)
    x$setinv(inv)
  }
  inv
}
