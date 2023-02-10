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
#
#
## cacheSolve(): calculates the inverse of a special matrix
##               (1) It first tests whether or not the function is a square invertible
##                   matrix
##               (2) If the inverse matrix holder is NOT NULL, it will get the cached
##                   inverse matrix value
##               (3) But if the inverse matrix holder is NULL, it will perform matrix
##                   inversion
#
cacheSolve <- function(x, ...){
  # installs "matrixcalc" package for checking the singularity of a matrix
  if(require("matrixcalc") == FALSE){
    install.packages("matrixcalc")
    library(matrixcalc)
  }
  
  # checks whether or not the argument is a square invertible matrix
  if(!!is.square.matrix(x$get()) & is.singular.matrix(x$get())){ 
    stop("\n\nWarning: The argument must be a square invertible matrix!\n\n")
  }
  
  ## gets the special "matrix" returned by the makeCacheMatrix()
  inverse_mat <- x$getInverse()
  
  ## If inverse_mat is NOT NULL, it will get the value of inverse_mat
  if(!is.null(inverse_mat)){
    message("Getting cached data")
    return(inverse_mat)
  }
  
  ## gets the matrix argument inputted in the makeCacheMatrix()
  data <- x$get()
  
  ## Solves the inverse of the special matrix
  inverse_mat <- solve(data, ...)
  
  ## assigns the inverse of the special matrix in the inverse_mat variable
  x$setInverse(inverse_mat)
  inverse_mat
}