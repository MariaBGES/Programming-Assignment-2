## Programming Asignment 2: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

## The first function is "makeCacheMatrix". This function
## creates a list containing other functions to:
##        setmatrix - Create and set the value of a matrix from the input
##        of the first function.
##        getmatrix - Get the value of the matrix.
##        setinverse - Set the inverse matrix.
##        getinverse - Get the inverse matrix.

## Example:
##   exmatrix <- makeCacheMatrix(matrix(1:4, 2, 2))

makeCacheMatrix <- function(x = matrix()) {
  minverse<- NULL
  
  setmatrix <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  getmatrix <- function() x
  
  setinverse <- function(solve) minverse <<- solve
  
  getinverse <- function () minverse
  
  list(setmatrix=setmatrix, 
       getmatrix=getmatrix,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The second function is "cacheSolve". This one calculates the inverse of
## the matrix created by "makeCacheMatrix". If the result has been calculated before
## and stored in the cache, the function "return" takes back the result.

## Example:
## cacheSolve(exmatrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minverse <- x$getinverse()
  if(!is.null(minverse)) {
    message("Getting cached data")
    return(minverse)
  }
  
  data <- x$getmatrix()
  minverse <- solve(data, ...)
  x$setinverse(minverse)
  minverse
}
