## R programming: week 3
## Assignment2: Caching the inverse of a matrix
## Example:
## x<-makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(x)

## The makeCacheMatrix creates a special "matrix": a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. setinv: set the value of the inverse matrix
## 4. getinv: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invm <<- inv
  getinv <- function() invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the following function calculates the inverse of a special matrix, 
## created with the makeCacheMatrix function
## Step1: Check if the inverse of the matrix has already been calculated
##        If TRUE : it gets the inverse from the cache, returns this variable
##                  skiping the computation
## Step2: Otherwise: Compute the inverse of the matrix and 
##                   set the value using setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("Already calculated data: getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinv(invm)
  invm
}
