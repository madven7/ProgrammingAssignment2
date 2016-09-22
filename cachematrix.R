## Put comments here that give an overall description of what your
## functions do
## These functions allow to cache and retrive the inverse of a matrix

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
# set sets the matrix
# get returns the cached matrix
# setinv sets the matrix inverse 
# getinv gets matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  # this is the matrix inverse, initialized to NULL
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setinv <- function(solve) r <<- solve
  getinv <- function() r
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  r <- x$getinv()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setinv(r)
  r
}

