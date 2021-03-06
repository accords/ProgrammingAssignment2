## Caching the Inverse of a Matrix

##  'makeCacheMatrix': 
##          This function creates a special "matrix" object that can cache its inverse.
##          There is no error catching controll, therefore it is expected that every matrix can be .
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {          
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## 'cacheSolve': 
##          This function computes the inverse of the special "matrix" returned by 'makeCacheMatrix' above. 
##          If the inverse has already been calculated (and the matrix has not changed), 
##          then the 'cachesolve' retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-solve(data, ...)
  x$setmatrix(m)
  m
}
