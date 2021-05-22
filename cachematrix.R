## Two functions that cache the inverse of a matrix to avoid having to recompute 
## it repeatedly. 
## If the inverse of a matrix has already been calculated, then the inverse is 
## retrieved from cache. Otherwise, the inverse is computed and cached.


## makeCacheMatrix creates a special "matrix": a list containing a function to 
## set and get the value of the matrix and to set and get the value of its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of the special "matrix" x has already 
## been calculated. If so, it retrieves the inverse from the cache. If not, it 
## calculates the inverse of the matrix and sets the value of the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
