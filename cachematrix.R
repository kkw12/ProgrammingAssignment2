## This function creates a special "matrix" object that can cache its inverse

## Below are two functions that are used to create a special object that stores 
## a matrix and cache's the inverse of a matrix.

## The first function, makeVector creates a special "matrix", which is really a 
## list containing a function to
##    1.set the value of the matrix
##    2.get the value of the matrix
##    3.set the value of the Solve (the inverse of matrix)
##    4.get the value of the Solve (the inverse of matrix)



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve <- function() m
  list(
    set = set, 
    get = get,
    setSolve = setSolve,
    getSolve = getSolve
  )

}




## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it  get s the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the  setSolve  function. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getSolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
  }
  
  
