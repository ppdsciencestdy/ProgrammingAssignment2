## The following two functions make it possible to cache  the inverse of a
## given matrix.  
## Given a matrix "X" whose inverse need to be found ( the source matrix), following functions
## allow the option of avoiding the costly inverse calculation if the source matrix 
## does not change. Functions work by storing the source matrix and its inverse on 
## first call and using the cached value from further calls
## Sample usage
## cfunction<-makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(cfunction)




## makeCacheMatrix , creates a list of functions 

## setInverse is the  function which  stores the
## inverse of the matrix in a variable inside its environment, and caches the data
## The variable matrixInverse contains this inverse , if available.
## getInverse is the function which can be used to get the inverse
## set is the function which caches the source matrix and resets the inverse cache, 
## resulting in the recalculation of inverse
## get is the function which can be used to get source matrix, if desired


makeCacheMatrix <- function(x = matrix()) {
  
  # Reset the inverse matrix cache on function defintion
  matrixInverse <- NULL
  
  # Set function , used to reset the inverse cache and set the source cache
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  
  # get function to provide source matrix when it is required
  get <- function() x
  
  # set the inverse matrix cache 
  setInverse <- function(Inverse) matrixInverse <<- Inverse
  
  # provide the cached inverse matrix when required
  getInverse <- function() matrixInverse
  
  
  # the special list which allows the functions to be called by external function
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve function when provided with the function object x of makeCacheMatrix(y)
## provides the inverse of y

cacheSolve <- function(x, ...) {
  
 
  # Get the inverse from cache
  mInverse <- x$getInverse()
  
  # if exists in cache return that instead of recalculating
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  
  # if not in cache , get the source matrix and recalculate
  sourceMatrix<- x$get()  
  mInverse <- solve(sourceMatrix, ...)
  
  # Set the cache with new inverse matrix
  x$setInverse(mInverse)
  
  # return the new inverse matrix
  mInverse
}
