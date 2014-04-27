## Functions to calculate the inverse of a square martrix, and 
## and cache the result.  Subsequent calls use cached data if the matric hasn't changed.
#  Test calls: 
#     x <- makeCacheMatrix(matrix(c(4,3,2,1),nrow=2,ncol=2))
#
#     cacheSolve(x)  #prints inverse, without message
#     cacheSolve(x)  #prints with mnessage about cached data
#
#     ada <- makeCacheMatrix(matrix(c(4,3,2,1,5,6,7,8,9),nrow=3,ncol=3))
#
#     cacheSolve(ada)  #prints inverse, without message
#     cacheSolve(ada)  #prints with mnessage about cached data
#
## "function" which is a list of functions to get and set the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## function to either calculate the inverse of a matrix, or return the cached value if it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
