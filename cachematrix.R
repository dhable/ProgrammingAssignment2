## These functions implement a simple memoize pattern on matrix data
## to cache inverse operations on the matrix so subsequent calculations
## only take O(1) to return.
##
## Sample Usage (thanks to Chris Craig for the sample):
##   a <- matrix(c(2,4,3,1), nrow=2, ncol=2, byrow=TRUE)
##   A <- makeCachedMatrix(a)
##   cacheSolve(A)
##   cacheSolve(A)
##
## Both calls to cacheSolve return the inverse matrix but
## only the first call needs to actually compute the inverse.


## Creates a matrix cache object that can be used to save operations on large
## matrixes. The object contains four method:
##    get(), which returns the raw matrix data that was wrapped in our class
##    set(), which allows you to change the underlying raw matrix data
##    getinverse(), which returns the matrix inverse cached value or NULL 
##        if not set
##    setinverse(), which updates the matrix inverse cache
##
## Arguments
##  x is the matrix data type to wrap up in the cached object. If a matrix
##  is not defined, one will be created for you.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedInverse <<- inverse 
  getinverse <- function() cachedInverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Uses our matrix cache wrapper object to cache inverse computations so
## subsequent calls for the calculation can return in constant time, using
## the same result created in the previous calculation.
##
## Arguments:
##  x is the matrix cache object obtained with makeCacheMatrix
##
## This method uses the solve() function and all the additional parameters
## are passed into solve() as is.
##
## Returns:
##  The inverse matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("using cached result")
    return(i)
  }
  matrixData <- x$get()
  computedInverse <- solve(matrixData, ...)
  x$setinverse(computedInverse)
  computedInverse
}
