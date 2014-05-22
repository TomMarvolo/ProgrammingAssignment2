makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(value){
    m <<- value
    inv <<- NULL
  }
  get <- function() m
  
  getInverse <- function() inv
  setInverse <- function(invese){
    inv <<- inverse
  }
  
  list( Set = set, Get= get, SetInverse=setInverse, GetInverse = getInverse)
}

cacheSolve <- function(m){
  i <- m$GetInverse()
  if(!is.null(i)){
    message("Obtaining data from Cache...")
    return(i)
  }
  matriz <- m$Get()
  i <- solve(matriz)
  m$SetInverse(i)
  return(i)
} 