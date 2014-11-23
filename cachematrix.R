## Coursera R Programming Assignment2: 
## two functions that can cache the inverse of a matrix.

## This makeCacheMatrix function creates a special matrix and caches the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This cacheSolve function computes the inverse of matrix returned by makeCacheMatrix.
## Checking if the inverse of matrix has already been calculated.
## If has been calculated, getting the inverse from the cache and skipping computation.
## If not, calculating the inverse and setting the inverse in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
