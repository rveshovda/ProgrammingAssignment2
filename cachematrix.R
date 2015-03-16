## This module has a special matrix that is capable of caching it's own inverse
## The module also has an function operating on the cachable matrix

## This creates a cachable matrix, which is capable of storing it's own inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(m){
    x <<- m
    inverse <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setInverse <- function(i){
    inverse <<- i
  }
  
  getInverse <- function(){
    inverse
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function tries to solve the inverse of a matrix. It uses the cached version if it exists.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setInverse(m)
  
  m
}
