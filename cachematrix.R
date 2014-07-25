## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a Matrix and return a vector which is really a list 
## containing a function to:
## set the value of the Matrix
## get the value of the Matrix
## set the Inverse of the matrix
## get the Inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setMatrix <- function(matrix) m <<- matrix
      getMatrix <- function() m
      list(set = set, get = get,
           setMatrix = setMatrix,
           getMatrix = getMatrix)
}

## cacheSolve takes a matrix and check if the inverse matrixhas already be created 
## cached, it returns the cached matrix if it is cached or solve for the inverse
## matrix and return it if it has not already been solved.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getMatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setMatrix(m)
      m
}

