## Put comments here that give an overall description of what your
## functions do

##This file consists of 2 functions makeCacheMatrix and cacheSolve 

## makeCacheMatrix consists of inv, set, setInverse, and getInverse and produces a list.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} #function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##This function gets the cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {               #checking if inverse is NULL
    message("getting cached data")
    return(inv)                     #returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)  #calculates inverse value
  x$setInverse(inv)
  inv  #returns a matrix that is the inverse of x
}
