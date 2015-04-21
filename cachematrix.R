

## This function creates a matrix object which has the ability to 
## calculate the inverse of the matrix and cache the inverse 
## Everytime the matrix changes the inverse of the matrix is 
## set to NULL, using cacheSolve() it has to be calculated and cached

makeCacheMatrix <- function(my_mat = matrix()) {
  
  ## initialize function variable 'my_mat' to empty matrix
  ## The variable 'inv' contains the inverse of the my_mat; 
  
  ## set inv to NULL at the start
  inv <- NULL
  
  ## This is the set function, use this to assign a matrix 
  ## for which inverse is needed  and 
  ## Set "inv" to NULL as my_mat has changed
  setmatrix <- function(y) {
    my_mat <<- y
    inv <<- NULL
  }
  
  ## The access function is named 'getmatrix'
  ## This will return the matrix thta was set using setmatrix
  getmatrix <- function() my_mat
  
  ## Assign the inverse of the matrix
  setinverse <- function(inv_mat) inv <<- inv_mat
  
  ## Access function to retrieve the inverse of the matrix
  getinverse <- function() inv
  
  list (setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
  
}



## This function is used to calculate the inverse of the matrix
## It uses the solve() function to acheive this

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Find out if the cached copy is already presnt.
  ## If present then return the inverse matrix that was already cached 'inv'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## The variable 'inv' is NULL so inverse is not yet calculated. 
  ## need to caluclate it now using solve(). 
  
  ## First get the matrix to 'data'
  data <- x$getmatrix()
  inv <- solve(data, ...)   ## use solve to get the inverse of the matrix
  
  ## USe the setinverse function to assign a value to the inv
  x$setinverse(inv) 
  
  ## Return the inverse
  inv
}
