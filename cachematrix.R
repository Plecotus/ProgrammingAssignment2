## cachematrix.R
##
## Functions for calculating the inverse of a matrix,
## and caching the result for later use.

## Creates a 'cache' for a matrix passed as argument.
## Returns a vector containing handles to 
## the data in the matrix, and the inverse of the matrix.

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
       setinverse = setinverse,
       getinverse = getinverse)
}


## For a given 'cache' vector (as specified in the previous function),
## this function retrieves the cached inverse of the matrix, 
## or calculates it if the cache is empty.
## Returns the inverse of a matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

