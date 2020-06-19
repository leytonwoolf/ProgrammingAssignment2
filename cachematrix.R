## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an R object that stores a matrix and its inverse in cache 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list (set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks if the inverse of input matrix is stored in cache.
## If not, function will calculate inverse of input matrix using solve().

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
