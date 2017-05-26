## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(y = matrix()) {

  inversefunc <- NULL
  set <- function(J) {
    y <<- J
    inversefunc <<- NULL
  }
  get <- function() y
  setInverse <- function(inverse) inversefunc <<- inverse
  getInverse <- function() inversefunc
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversev <- x$getInverse()
  if (!is.null(inversev)) {
    message("getting cached data")
    return(inversev)
  }
  y <- x$get()
  inversev <- solve(y, ...)
  x$setInverse(inversev)
  inversev
}
