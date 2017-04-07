## Caching the Inverse of a Matrix:
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix" created by CacheMatrix above. If the inverse has already been calculated (and the 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInverse()
  mat <- x$get()
  inv_matrix <- solve(mat, ...)
  x$setInverse(inv_matrix)
  inv_matrix
}

