#creates a special matrix object that can cache its inverse  
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  #set/modify the value of the matrix, 'reset' the stored value of inverse; get the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL #sets inverse back to NULL if the matrix is changed
  }
  get <- function() x
  
  #set/cache the value of inverse; get the value of inverse
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  
  #return an object of type makeCacheMatrix to be used by cacheSolve
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


#computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x) {
  #get the value stored 
  inverse <- x$getinverse()
  
  #if the inverse in NOT null, then retrieve the inverse from the cache
  if(!is.null(inverse)) { 
    message("cached inverse of matrix")
    return(inverse)
  }
  #get & store the matrix, get & store the inverse of the matrix, cache the inverse, print the result 
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}