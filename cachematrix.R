## Abstract / Executive Summary
## The makeCacheMatrix function caches/persists matrix and inverse matrix values through set 
## functions and returns those matrices through corresponding get functions.

## The cacheSolve requests the inverse of a matrix stored/cached in the makeCacheMatrix
## function.

## If the matrix returned  by the makeCacheMatrix$getinvMatrix function is a NON-EMPTY matrix 
## the cacheSolve function then prints a message indicating that a cached (inverse) matrix 
## was returned and then terminates its own execution by returning the cached inverse matrix.

## If the matrix returned by the makeCacheMatrix$getinvMatrix function is an EMPTY matrix 
## then the cacheSolve fucntion:
## 1) Requests the matrix to be inverted by way of the makeCacheMatrix$getMatrix function
## 2) Solves for the inverted matrix by way of the base R solve() function
## 3) Caches the solution by way of the makeCacheMatrix$setinvMatrix function

##########################################################################
## FUNCTION NAME: makeCacheMatrix
## FUNCTION PURPOSE: To store matrix and inverse matrix values so that they may
##                   be randomly accessed (returned) by other functions and code blocks
##                   outside of the makeCacheMatrix.  In other words, this function
##                   maintains the state of matrix and inverse matrix values.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Instantiate an empty (0,0) matrix
  invMatrix <- matrix(nrow = 0, ncol = 0)
  
  ## Object method for setting a new matrix object
  setMatrix <- function(newMatrix) {
    
    ## Assign new Matrix object to the instance of the makeMatrix object created
    x <<- newMatrix
    
    ## Reinitialize the Inverse Matrix object since the matrix on which it is based has changed.
    invMatrix <<- matrix(nrow = 0, ncol = 0) 
  }
  
  
  ## Return the matrix stored in the makeMatrix object
  getMatrix <- function() x
  
  ## Set the inverse matr
  setinvMatrix <- function(im) {
    invMatrix <<- im
  }
  
  ## Return the Inverse Matrix object
  getinvMatrix <- function() invMatrix 
  
  ## Return the interface of methods provided by the makeMatrix object
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setinvMatrix = setinvMatrix, 
       getinvMatrix = getinvMatrix)
}

##########################################################################
## FUNCTION NAME: cacheSolve
## FUNCTION PURPOSE: Return a matrix that is the inverse of 'x'
##                   If the inverse matrix value/property from the makeCacheMatrix function
##                   is populated then the cached value will be return.  Otherwise the 
##                   inverse of the matrix passed to cacheSolve will be calculated.
cacheSolve <- function(x, ...) {
  ## Instantiate an empty (0,0) matrix
  invMatrix <- matrix(nrow = 0, ncol = 0)
  
  ## Call the getinvMatrix on the instance of the makeMatrix object created
  invMatrix <- x$getinvMatrix()
  
  ## Test if matrix is empty
  if ((nrow(invMatrix) != 0) & (ncol(invMatrix) != 0))
  {
    message("getting cached matrix")   ## return message to console
    return(invMatrix)  ## return the Inverse Matrix object and exit cacheSolve function
  }
  
  ## Get matrix from instance of the makeMatrix object created
  data <- x$getMatrix()
  
  ## Calculate the inverse of the matrix
  invMatrix <- solve(data)
  
  ## Set the invMatrix property of the instance of the makeMatrix object created
  x$setinvMatrix(invMatrix)
  
  ## return the Inverse Matrix object
  invMatrix
}
