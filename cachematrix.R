## This couple of functions compute the inverse of (an ivertible matrix)
## by checking whether the inverse in already in cache or not 

## Creates matrix structure for inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Compute matrix inverse 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat.data <- x$get()
  m <- solve(mat.data, ...)
  x$setinv(m)
  return(m)
}
