## The two functions below were written in order to cache the inverse of an
## assumed square matrix. 

## The 'makeCacheMatrix' function creates a matrix that can cache its inverse. 
## This means that it creates a matrix and stores its mean. It is assumed that 
## the matrix supplied is always a square, invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## The 'cacheSolve' function will return the inverse of the matrix that was 
## input into the 'makeCacheMatrix' function. It will retrieve the inverse from 
## the cached value stored in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i     
  
}
