## This script defines a Matrix which save its own inverse.

## makeCacheMatrix -> creates a matrix with a Setter/Getter for the values
## and Setter/Getter for the inverse values.

makeCacheMatrix <- function(x = matrix()) { # if x is missing create an empty matrix
  inv <- NULL
  set <- function(value){
    x <<- value
    inv <<- NULL
  }
  get <- function() x
  
  getInverse <- function() inv
  setInverse <- function(inverse){
    inv <<- inverse
  }
  
  list( Set = set, Get= get, SetInverse=setInverse, GetInverse = getInverse)
}


## cacheSolve -> calculates inverse matrix of x, 
## arguments:
## x -> Matrix, create with makeCacheMatrix function.
## ... -> additional parameters for solve Function.

cacheSolve <- function(x, ...) {
  
  i <- x$GetInverse()
  
  if(!is.null(i)){
    message("Obtaining data from Cache...")
    return(i)
  }
  
  matriz <- x$Get()
  
  i <- try(solve(matriz, ...))
  m$SetInverse(inverse = i)
  
  return(i)
}
