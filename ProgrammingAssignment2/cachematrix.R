
##This function takes a matrix and caches its inverse.
## it also creates four functions
## set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #cache
  set <- function(y) {
    x <<- y    # Set the value
    m <<- NULL # Clear the cache
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of a
## matrix returned by makeCacheMatrix. 
## If the inverse exists (and the matrix has not changed), 
## then this function should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() # check cache for inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if cache is empty, find inverse and cache result
  data <- x$get()  
  m <- solve(data) 
  x$setInverse(m)  
  m                
}
