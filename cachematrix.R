##  This script contains two main functions used to create an object that 
##  stores a matrix and cache's its inverse.

##  The first main function, makeCacheMatrix, creates a list with a nested 
##  function to perform four steps:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse

##  this list becomes the input for the second function, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    
    inv = NULL
    set = function(y) {
    x <<- y
    inv <<- NULL
  }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##  The second main function, cacheSolve, performs two tasks:
##    1. Determines if the matrix is already inverted; if not,
##    2. The matrix is inverted.

cacheSolve <- function(x, ...) {
    
    inv = x$getinv()

    if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Invert the matrix
    
    mat.data = x$get()
    inv = solve(mat.data, ...)
  
  # cache's the inverse via the setinv function.
  
    x$setinv(inv)
    inv
}
