## The following functions take a matrix as an argument and checks if its inverse has already been
## calculated, in which case it will be found stored in cache. If so, it returns the (cached) 
## inverse matrix. If not, it calculates and returns the inverse matrix (and also caches its value).

## This function will generate a list of four other functions - set, get, setinverse and getinverse -
## which will work on the desired matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will first check if the inverse matrix is already stored in cache. If so, it will
## return the cached value. If not, it will calculate and return the inverse matrix, and also cache
## its value.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
