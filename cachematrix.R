## The following functions allow the storage of the calculation result of the cache of an 
## inverse matrix, allowing the reuse of information without the need to recalculate again.
##
## Author: Cristian Julio de Barros
## Date:   21/02/2015



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matInversa <- NULL
  set <- function(y) {x <<- y; matInversa <<- NULL}
  get <- function() x
  setinversa <- function(inversa) matInversa <<- inversa
  getinversa <- function() matInversa
  list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)
  
}


## This function computes the inverse of the  special "matrix" returned by  makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  matInversa <- x$getinversa()
  if(!is.null(matInversa)) {
    message("getting cached data")
    return(matInversa)
  }
  data <- x$get()
  m <- solve(data)
  x$setinversa(m)
m
}  	
