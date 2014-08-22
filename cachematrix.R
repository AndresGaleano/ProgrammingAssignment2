## This function will create a matrix and obtain its inverse. If the inverse was already obtained in previous runs,
## the function will pull the inverse from the environment.

## The function makeMatrix creates a "matrix" which is actually a list with four functions
makeMatrix <- function(x) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) Inv <<- solve
  getinv <- function() Inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This second function uses the matrix you creatd using makeMatrix to compute its inverse. If it was already calculated
## before, the function will return the stored result.

cacheSolve <- function(x, ...) {
  Inv <- x$getinv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinv(Inv)
  Inv
}
