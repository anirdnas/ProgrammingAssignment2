## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## set: to set up the matrix and reset cach
## get: see the matrix
## setValue: to save the solve value
## getValue: see the cached value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setValue <- function(solve) {
    m <<- solve
  }
  getValue <- function() {
    m
  }
  list(set = set, get = get, setValue = setValue, getValue = getValue)
}

## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.
##SetValue is the method for saving into cache. 

cacheSolve <- function(x, ...) {
  m <- x$getValue()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setValue(m)
  m
}