## Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}

## Test case for getting cached data 

cacheSolveTest <- function(matrix = matrix( c(2, 4, 3,  1, 5, 7,  7, 4, 1), nrow=3, ncol=3 )) {  
  message("cache = makeCacheMatrix(x)   #initializing cache")
  cache = makeCacheMatrix(x)
  message("cacheSolve(cache)            #first run: adding data to cache")
  cacheSolve(cache)
  message("cacheSolve(cache)            #second run: data from cache is returned")
  cacheSolve(cache)
}

