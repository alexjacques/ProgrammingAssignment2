## These two functions are supposed to take the inverse of matrices, or detect if previous functions
## have already done so in order to save time.

## The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()
    x
  setInverse <- function(Inverse)
    inv <<- Inverse
  getInverse <- function()
    inv
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
  
}

## The second function, cacheSolve, identifies if the special matrix has had its inverse taken already.
## If it has, it will return it. If not, it will operate on it and take the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}