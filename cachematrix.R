## makeCacheMatrix and cacheSolve functions are used in conjunction with each other to store a matrix object and its inverse. 
## inverse object once stored in makeCacheMatrix acts as cache so that subsequent calls to cacheSolve (after first call) 
## do not calculate inverse of the matrix untill the matrix object remains the same.


## This function is used to contain a matrix object and its inverse. Inverse is stored to use it as cache so that inverse 
## is not assigned to (is NULL) unless it is set. It has attribute functions to get and set matrix object as well as its inverse.

makeCacheMatrix <- function(obj = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    obj <<- y
    inverse <<- NULL
  }
  get <- function() obj
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## Pre-Condition: passed in matrix is square
## This function calculates inverse of the passed in matrix and caches it (using setInverse function of makeCacheMatrix) 
## for the first call. For all subsequent requests, it retrieves the inverse from cache using getInverse of makeCacheMatrix. 

cacheSolve <- function(obj, ...) {
  ## Returns a matrix that is the inverse of passed in (parameter) matrix object
  inverse <- obj$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- obj$get()
  inverse <- solve(data, ...)
  obj$setInverse(inverse)
  inverse
}
