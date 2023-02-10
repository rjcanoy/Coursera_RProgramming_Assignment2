## This function performs matrix inversion by caching the inverse of a matrix
## rather than computing it repeatedly
#
## =============================================================================
## Author: Raymart Jay Canoy
## Date: 03 September 2022
## =============================================================================
#
## makeCacheMatrix(): creates a special matrix that can cache its inverse and
##                    returns a list of four functions:
## myMatrix <- makeCacheMatrix(x)
## myMatrix$set(): sets the inverse matrix holder to NULL
## myMatrix$get(): gets the matrix argument
## myMatrix$setInverse(): sets the result of matrix inversion to the holder
## myMatrix$getInverse(): gets the result of matrix inversion
#
makeCacheMatrix <- function(x=matrix()){
  inverse_mat = NULL; # initializes the inverse matrix holder
  
  ## set() function
  set <- function(y){
    x <<- y
    inverse_mat <<- NULL
  }
  
  ## get() function
  get <- function() x
  
  ## setInverse() function
  setInverse <- function(inverse) inverse_mat <<- inverse
  
  ## getInverse() function
  getInverse <- function() inverse_mat
  
  ## returns
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}