# I
## This function makeCacheMatrix, creates a special matrix object 
#that can cache its inverse.
# makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinvmat <- function(solve) z <<- solve
  getinvmat <- function() z
  list(set=set
       , get=get
       , setinvmat=setinvmat
       , getinmat=getinmat)
}
# II
# The following function returns the inverse of the special "matrix". 
# It first checks if the inverse has already been computed. 
# If so, it gets the inverse as result and skips the computation.
# If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.

cashesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getinmat()
  if(!is.null(z)) {
    message("getting cashed data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinvmat(z)
  z
}
