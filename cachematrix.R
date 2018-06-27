## Functions for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "vector", 
  # which is really a list containing a function to:
  # 1. Set the value of the matrix
  # 2. Get the value of the matrix
  # 3. Set the value of the inverse
  # 4. Get the value of the inverse
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  } 
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x,...) {
  # Return a matrix that is the inverse of 'x'
  # Only calculates the matrix if it is not cached
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}