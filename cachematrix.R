## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function takes square matrix or no argument, returns four function, 
# those function  can be used to 
# to set a square matrix, retrieved the original matrix, 
# set the inverse of the square matrix and get cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  invx <-  NULL
  
  setMatrix <- function(y){  # setting the square matrix
    
    x <<- y
    invx <<- NULL
  }
  
  getMatrix <- function() x                       # getting the original matrix
  
  setInvMatrix <- function(invM) invx <<- invM       # set the inverse of the matrix
  getInvMatrix <- function() invx                  # returned the cached inverse of the matrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
  
}


## Write a short comment describing this function
# return the inverse of a matrix which stored in x. Here object x is created by
# using the function "makeCacheMatrix" above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getInvMatrix()
  if(!is.null(invx)){
    message("inverse from caches")
    return(invx)      # if it is already cached, then it will return the cached inverse
  }
  
  invx <- solve(x$getMatrix())         # if not cached, it determines the inverse
  x$setInvMatrix(invx)               # then , here it is cached
  invx
  
}

