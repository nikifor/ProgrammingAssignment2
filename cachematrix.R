## makeCacheMatrix and cacheSolve can be used in tandem to compute the inverse
## of an inversible squared matrix, and cache the result. In this way, if the
## programmer asks for the inverse of the same matrix, the result is fetched from
## the cache, instead of being recalculated

## makeCacheMatrix returns a list (creating an object-like feeling) which exposes
## four functions that can be used to read and write a matrix, as well as its
## inverse. The super assignment operator is used so that the assignments in the
## two encapsulated functions happen in their parent frame and thus persist


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve first checks if the inverse of the given matrix is already computed
## and if it is, it is returned. If not, the inverse is computed, cached, and returned

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
