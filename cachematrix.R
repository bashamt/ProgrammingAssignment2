## These two functions demonstrate lexical scoping in R with variables in the parent function being assigned values from
## the child function using the <<- operator

## This function creates a matrix which sets and gets the value of the matrix and sets and gets the value of the inverse 
## of the matrix.  This value will then be returned by running the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(get=get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function calculates the inverse of the matrix defined in makeCacheMatrix but will use the cached version if this exists. 
## If the calculation is completed (as there is no cache), it will populate the cache for later use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}