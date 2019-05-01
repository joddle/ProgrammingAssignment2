## The makeCacheMatrix function creates a list of functions... 
## ...to set and get the matrix, and set and get the inverse of the matrix

## The cacheSolve function calculates the inverse of the matrix created by makeCacheMatrix()...
## ... and checks if the inverse has been calculated already

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x 
  ## this gets the matrix x
  
  setInverse <- function() inv <<- solve(x) 
  getInverse <- function() inv
  
  ## sets the inverse using the solve function, and gets inverse
  
  list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function calculates the inverse of the matrix created 

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$getInverse()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
    
    ## Return a matrix that is the inverse of 'x'
}
