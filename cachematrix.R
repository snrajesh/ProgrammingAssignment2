### The functions below are used for computing inverse of a matrix and caching it for on-going use via special matrix

### Example:
### m1 <- makeCacheMatrix(matrix(1:4,2,2))   ## create (speicial) matrix m1 in memory 
### cacheSolve(m1);    ##  return inverse matrix of m1 (either from memory or compute it)

# Author : Rajesh
# Created/Modified: 09/25/15
############################################################################################

  ## function makeCacheMatrix:
  ##  The first function, creates a special matrix, which is really a list containing functions to
  ##    set the value of the matrix, get the value of the matrix
  ##    set the inverse of the matrix, and get the inverse of the matrix
  
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
  }
  
  
  ## Function cacheSolve:
  ##  This function retruns the inverse of a matrix 
  ##    First it looks at the special matrix to see if the inverse is available in the memory, if available returns that
  ##    if not it will compute the inverse matrix, store it in the memory (special matrix), and return the inverse matrix 
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    
  }
  
