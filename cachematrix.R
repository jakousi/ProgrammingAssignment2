## This file contains two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix - return a list with functions to access a matrix and
## its inverse
## cacheSolve - calculates the inverse of the matrix returned by makeCacheMatrix

## Return a list with a matrix (the argument x) and a variable (which will
## contain its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Inverse of the matrix x (not calculated yet)
  inversa <- NULL
  ## Set the matrix pass by argument and initialize its inverse
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  ## Return the matrix
  get <- function(){
    x
  }
  ## Set the inverse of the matrix
  setInversa <- function(inversa) {
    inversa <<- inversa
  }
  ## Return the inverse of the matrix
  getInversa <- function() {
    inversa
  }
  ## Return a list with 4 functions associated to the matrix
  list(set = set, get = get,
       setInversa = setInversa,
       getInversa = getInversa)
}


## Return the inverse of the matrix 'x', if not cached this function
## calculates it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInversa()
  ## Check for previous value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Not previous value, proceed to calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  ## Save in 'cache'the value of the x's inverse
  x$setInversa(inv)
  inv
}
