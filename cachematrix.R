## A test project for Coursera: write two functions 
## The first function creates a "special" matrix, the second one caches its inverse

## Creates a "matrix" object; 4 methods - get, set, setInverse, getInverse. Returnes a list of methods

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## function - set  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## function - get
  get <- function() x
  
  ## function - set the inverse
  setInverse <- function(solve) inv <<- solve
  
  ## function - get the inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Caches the inverse of the matrix if it has not been cached yet. Returnes the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  
  ## check if the matrix has already been cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Cache the matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
