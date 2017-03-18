## Functions that create cached inverse

## Function returns cached matrix if already calculated or calculates
## and stores matrix otherwise

makeCacheMatrix <- function(x = matrix()) {
  ## initialize cache to null
  m <- NULL
  ## setter function initializes cache to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## getter function
  get <- function() x
  ## assign inverse to cache
  setinverse <- function(inverse) m <<- inverse
  ## get cache
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that calculates inverse and sends a message when retrieving
## the cached version

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if cached, return cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## in not in cache, calculate
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
