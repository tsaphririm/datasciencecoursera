## the function makeCacheMatrix builds a list of four arguments which will work as
## cache for the values obtained from the inversion of a given matrix -- so that
## the inverted matrix does not need to be calculated again in case its values are
## repeatedly needed. The values are instead called from one of the compoments of
## the list built by this function.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {    
    x <<- y         ## the function "set" will "receive" the matrix to
    m <<- NULL      ## be solved as one of the list's components
  }
  
  get <- function() {     ## the function "get" will return the value that
    x               ## function "set" saved as "X"        
  }
  
  setinverse <- function(solve) { ## the function "setinverse" creates the
    m <<- solve             ## inverted matrix of "x" and allocates it        
  }                               ## to the variable "m"
  
  getinverse <- function() {      ## function "getinverse" returns the cached
    m                       ## value of the inverted matrix
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ## a list is created where the values of the original matrix as well as the
  ## inverted matrix are readilly available, without the need of repeating
  ## inversion calculation.
}

## Function "cacheSolve" will first proof if an inversion calculation of a given
## matrix has already been performed. In affirmative case, it should be stored as
## a componente of the list created by the function "makeCacheMatrix". If the 
## calculation has already been performed, "cacheSolve" will return the calculated
## value. If the calculation was not yet performed, "cacheSolve" will invert the
## given matrix AND assign it's value to the list of "cached" values, so it will
## be readilly available next time that the values of the inverted matrix are needed.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  
  m
  
}
