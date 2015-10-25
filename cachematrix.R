## pair of functions - makeCacheMatric and cacheSolve - used together create a matrix
# and make its inverse available in cache

##---------------------------------------------------------------
## creates and returns list of functions used by cacheSolve to set or get the invereted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
    ## initializ cache to Null, create matrix
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    ##set up functions and return to working environment
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
  ## Return matrix from cache if exisist
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Invert, cache and return if not cached already
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setMatrix(m)
  m
}
