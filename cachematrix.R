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
    
    # initialize inverse matrix
    im <- NULL
    
    # function to set/store the matrix and reset inverse matrix to NULL as the matrix was changes
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    
    # function to return the stored matrix
    get <- function() x
    
    # function to store the value of the inverse matrix in the main function
    setInverse <- function(solve) im <<- solve
    
    # function to return the value of the stored inverse matrix 
    getInverse <- function() im
    
    # To return all the 4 functions, as a list, so that they are accessible when we create a matrix with makeCacheMatrix to an object
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
  }
  
  
  
  ## Function cacheSolve:
  ##  This function retruns the inverse of a matrix 
  ##    First it looks at the special matrix to see if the inverse is available in the memory, if available returns that
  ##    if not it will compute the inverse matrix, store it in the memory (special matrix), and return the inverse matrix 
  
  cacheSolve <- function(x, ...) {
    
    ## get inverse matrix of special matrix that is created via makeCacheMatrix and stored in the main function 
    im <- x$getInverse()
    
    ## if the value of the inverse matrix returned from the main function is NOT Null return that value
    ##  (this means value is previously calculated and saved in the main function)
    if(!is.null(im)) {
      message("getting cached data")
      # return the value of inverse matrix (and exit)
      return(im)
    }
    
    ## if the value of the inverse matrix returned is Null then compute inverse matrix
    
    ## get the matrix from the main function
    data <- x$get()
    
    ## compute the inverse of the matrix using the "solve" funtion
    im <- solve(data, ...)
    
    ## set/save the value of the inverse matrix to the main function
    x$setInverse(im)
    
    ## return inverse matrix
    im
    
  }
  
