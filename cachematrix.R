## Put comments here that give an overall description of what your
## functions do

## This function defines a vector of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

## Set the value of the inverse to null
  i <- NULL
  
  ## set the matrix and clear the inverse (since it is a new matrix)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## return the matrix
  get <- function() x
  
  ## set the stored inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## retrieve the stored inverse
  getinverse <- function() i
  
  ## return the vector of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculated the inverse of a matrix.  
## If the inverse already exists in cache, use it
## otherwise return the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## retrieve the cache to determine if the inverse is already stored
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## It is not already stored, so get the matrix, calculate the inverse and save it in cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  ## return the inverse
  i
}
