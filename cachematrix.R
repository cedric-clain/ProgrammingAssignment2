## The aim of the functions below are to calculate the inverse of a matrix
## and make sure that if the inverse has already been calculated, 
## it retrieves it from a cache instead of recalculating it.

## The function makeCacheMatrix() creates a list containing four functions:
## set: to set the matrix
## get: to get/return the matrix
## setinverse: to set the inverse of the matrix
## get inverse to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list (set = set, get = get, setinverse = setinverse, 
        getinverse = getinverse)
  
}

## The function cacheSolve() returns the inverse of a matrix.
## If the matrix has already been calculated, it retrieves it from a cache.
## Else, it calculates it and saves it to the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
