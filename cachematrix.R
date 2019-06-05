#creates a special matrix object that can cache its inverse  
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  #set/modify the value of the matrix, and then 'reset' the stored value of inverse; get the value of the matrix
  set.matrix <- function(y) {
    x <<- y
    inverse <<- NULL 
  }
  get.matrix <- function() x
  
  #set (cache) the value of inverse; get the value of inverse
  set.inverse <- function(inverse) inverse <<- inverse
  get.inverse <- function() inverse
  
  #return an object of type makeCacheMatrix to be used by cacheSolve
  list(set.matrix = set.matrix, get.matrix = get.matrix, 
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


#computes the inverse of the matrix returned by makeCacheMatrix
cacheSolve <- function(x) {
  #get the value stored 
  inverse <- x$get.inverse()
  
  #if the inverse is NOT null, then retrieve the inverse from the cache
  if(!is.null(inverse)) { 
    message("cached inverse of matrix")
    return(inverse)
  }
  #get & store the matrix, get & store the inverse of the matrix, cache the inverse, print the result 
  data <- x$get.matrix()
  inverse <- solve(data)
  x$set.inverse(inverse)
  inverse
}