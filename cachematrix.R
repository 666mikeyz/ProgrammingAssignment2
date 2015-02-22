## makeCacheMatrix creates a 'matrix' object that can cache
## the result of the solve function (the inverse). The matrix
## object is a list containing getters and setters for the 
## matrix and matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
	m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix.
## If a cached inverse result is available then the cached result is returned,
## otherwise the solve function is called.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message('Retrieving cached data...')
	return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
