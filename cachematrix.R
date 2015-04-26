## These functions have been created for RPROG course. One is a function that sets
## and gets matrix value then sets and gets inverse value. Second function caches the
## the inverse of the matrix if it has been solved before, and calculates a new inverse
##and caches if the matrix has not been solved before.

## This function is similar to the makeVector function of the example.
##It is responsible for setting and getting the matrix value and set and getting
##the inverse of the matrix value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  printm <- function() print(is.null(m))
  
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse, printm=printm)
  
}


## caches the the inverse of the matrix if it has been solved before, 
##and calculates a new inverse
##and caches if the matrix has not been solved before


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

