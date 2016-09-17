## makeCacheMAtrix saves the matrix and if the inverse has already been calculated,it will save the result as well

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {                 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cachesolve checks if the matrix's inverse has been calculated by looking into the parent environment and 
##if it is saved in the cache,it will retrieve it, else it will calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
