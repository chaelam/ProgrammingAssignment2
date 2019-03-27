## Put comments here that give an overall description of what your
## functions do

## This code sets and gets the value of a matrix, and also the inverse of it. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #gets the value of the matrix
  get <- function() x
  #calculates the inverse of the matrix
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  #passes the values to the cacheMatrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function calculates and returns the inverse of the matrix using the function 
# defined previously.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  #checks if passed matrix is empty
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

mat<-matrix(c(1,4,9,0,-3,2,2,7,8),3,3) 
x<-makeCacheMatrix(mat)
cacheSolve(x)
