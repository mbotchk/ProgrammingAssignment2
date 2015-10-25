## pair of functions - makeCacheMatric and cacheSolve - used together create a matrix
#and make its inverse available in cache

##---------------------------------------------------------------
## creates and returns list of functions used by cacheSolve to set or get the invereted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setMatrix <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getInverse = getInverse)
  }
  
##-------------------------------------------------------------------
## computes the inverse of the matrix created in makeCacheMatrix
## if current inverse available in cache uses cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setMatrix(m)
  m
}
