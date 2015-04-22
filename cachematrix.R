## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

cacheSolveTest <- function(matrix = matrix( c(2, 4, 3,  1, 5, 7,  7, 4, 1), nrow=3, ncol=3 )) {  
  message("cache = makeCacheMatrix(x)   #initializing cache")
  cache = makeCacheMatrix(x)
  message("cacheSolve(cache)            #first run: adding data to cache")
  cacheSolve(cache)
  message("cacheSolve(cache)            #second run: data from cache is returned")
  cacheSolve(cache)
}
