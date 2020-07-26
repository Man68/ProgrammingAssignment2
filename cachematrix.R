## The function below is creating a cached matrix that stores the data x coming
## from the call makeCacheMatrix.
## the set nested function is dedicated to store the original value of the matrix
## the same function states the inversedmatrix as NULL (setup of inversed matrix)



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversedmatrix <- NULL
  
  ## the sematrixt nested function is dedicated to store the original value of the matrix
  ## the same function states the inversedmatrix as NULL (setup of inversed matrix)
  
  setmatrix <- function(matr) {
    x <<- matr
    inversedmatrix <<- NULL
  }
  ## getmatrix function is required to retrieve the original matrix value
  getmatrix <- function() x
  
  ## setinversedmatrix is setting the values of the inversedmatrix from the matrix
  setinversedmatrix <- function(solve) inversedmatrix <<- solve(x)
  
  ## getinversedmatrix is required to retrieve from the cache the inversed matrix
  getinversedmatrix <- function() inversedmatrix
  
  ##Last value returned form the function : list of functions to store/retrieve data 
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinversedmatrix = setinversedmatrix,
       getinversedmatrix = getinversedmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Cached matrix is retrieved only if it exists AND
        ## if the matrix analyzed is the same as the one from which the cache comes
        ## Otherwise, the inversed matrix is recalculated
  originmat<-x$getmatrix()
  mat <- x$getinversedmatrix()
  
  if (!is.null(mat)) {
    message("getting inverted matrix from cached data")
    return(mat)
  }
  message("calculating the inverse matrix")
  mat <- solve(originmat, ...)
  x$setmatrix(mat)
  mat
  
}
