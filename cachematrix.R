## the function makeCacheMatrix builds a list of four arguments which will work as
## cache for the values obtained from the inversion of a given matrix -- so that
## the inverted matrix does not need to be calculated again in case its values are
## repeatedly needed. The values are instead retrieved from one of the compoments of
## the list built by this function.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {    
    x <<- y         
    m <<- NULL      
  }
  
  get <- function() {     
    x             
  }
  
  setinverse <- function(solve) { 
    m <<- solve                   
  }                               
  
  getinverse <- function() {    
    m                             
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ## a list is created where the values of the original matrix as well as the
  ## inverted matrix are readilly available, without the need of repeating
  ## inversion calculation.
}

## Function "cacheSolve" will first proof if an inversion calculation of a given
## matrix has already been performed and stored in the list created by the function 
## "makeCacheMatrix". If the calculation has already been performed, "cacheSolve" 
## will retrieve the calculated value from the list.
## If the calculation was not yet performed, "cacheSolve" will invert the
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
