## makeCacheMatrix & cacheSolve work in combination to create and store in cache
## a given matrix and its inverse.  If inverse is not stored in cache, it will be
## created and stored in cache.


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
