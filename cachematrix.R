## This code is used to create a matrix, which can also obtain its inverse. The two functions used
## here are makeCacheMatrix and cacheSolve. 

## This function creates a vector containing a function to set the value and get the value of the
## vector, as well, get the value of the inservse and set the inserve.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function is used to compute the inverse of the vector and should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getinverse
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$get
        m <- inv(data, ...)
        x$setinverse(m)
        m
}
