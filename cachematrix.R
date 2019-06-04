#creates a special matrix object that can cache its inverse  
makeCacheMatrix <- function(x = matrix()) {
  original <- NULL
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setoriginal <- function(y) {
    original <<- y
  }
  getoriginal <- function() original
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get, getoriginal = getoriginal, setoriginal = setoriginal,
       setinverse = setinverse,
       getinverse = getinverse)
}


#computes the inverse of the special matrix returned by makeCacheMatrix
#if the inverse has already been calculated (and the matrix has not changed), then retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  original <- x$getoriginal()
  # Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse) && !is.null(original) && all(original == x$get())) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  x$setoriginal(data) #save the original
  m <- solve(data)
  x$setinverse(m)
  m
}