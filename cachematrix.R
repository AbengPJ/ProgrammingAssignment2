# R Programming Assignment #2
# submitted by ChorHuat
#
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  out <- tryCatch(solve(data)%*%data, error = function(e) e)
  
  if (any(class(out) == "error")) {
    i<-"The matrix is singular!"
  } else {
    i <- solve(data, ...)
  }
  x$setinverse(i)
  i
}
