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
  
  if(!is.null(i)){  ## If isn't the first time you calculate the inverse return the value of i
    message("Obtaining data from Cache...")
    return(i)
  }
  ## If its the first time
  matriz <- x$Get()  ## obtain the values of th ematrix
  
  i <- try(solve(matriz, ...)) ## calculate the inverse 
  x$SetInverse(inverse = i) ## and update the values saved in the matrix x
  
  return(i)
}
