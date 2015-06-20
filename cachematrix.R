## Caching the Mean of a Vector

## 1st function: it creates a list that contains 4 member

makeCacheMatrix <- function(x = matrix()) {
    M <- NULL # to store the inversion
    set <- function(b) { # A setter function 
      x <<- b 
      M <<- NULL 
    }
    
    get <- function() x # return the input
    setI <- function(I) M <<- I # set the inversed
    getI <- function() M # return the inverted
    
    list(set = set, get = get, setI = setI, getI = getI) # return a list that contains functions
}


## 2nd function: an assign, and if/else, set, and return

cacheSolve <- function(x, ...) {
  Mat <- x$getI() # get the inversed
  if(!is.null(Mat)) { # if exists
    message("getting cached data")
    return(Mat) # return inversion
  }
  data <- x$get() # else
  Mat <- solve(data) # solve
  x$setI(Mat) # set the inversed
  Mat # return
}
