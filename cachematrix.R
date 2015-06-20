## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    M <- NULL 
    set <- function(b) {
      x <<- b
      M <<- NULL 
    }
    
    get <- function() x 
    setI <- function(I) M <<- I 
    getI <- function() M 
    
    list(set = set, get = get, setI = setI, getI = getI)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  Mat <- x$getI()
  if(!is.null(Mat)) { 
    message("getting cached data")
    return(Mat)
  }
  data <- x$get() 
  Mat <- solve(data) 
  x$setI(Mat) 
  Mat
}