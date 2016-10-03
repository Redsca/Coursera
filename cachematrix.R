
## Changed the mean to INV
## Changed the mean function to Solve
## Both function write to same location

makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) INV <<- inverse
  getInverse <- function() INV
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  INV <- x$getInverse()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  Matrx <- x$get()
  INV <- solve(Matrx, ...)
  x$setInverse(INV)
  INV
}
