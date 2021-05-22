## Two functions that cache the inverse of a matrix to avoid having to recompute 
## it repeatedly. 
## If the inverse of a matrix has already been calculated, then the inverse is 
## retrieved from cache. Otherwise, the inverse is computed and cached.

## makeCacheMatrix creates a special "matrix": a list containing a function to 
## set and get the value of the matrix and to set and get the value of its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of the special "matrix" x has already 
## been calculated. If so, it retrieves the inverse from the cache. If not, it 
## calculates the inverse of the matrix and sets the value of the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
