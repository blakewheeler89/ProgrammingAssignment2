makeCacheMatrixTwo <- function(x = matrix()) {
  m <- matrix(x, nrow = sqrt(length(x)), ncol = sqrt(length(x)))
  cache_matrix <- NULL
  
  cache_inverse_matrix <<- solve(m) 
  m
}


## This function takes the cache_inverse_matrix (if available) or an input matrix as arguments and returns either the cache_inverse_matrix
## or the inverse of the input matrix.

cacheSolveTwo <- function(x, ...) {
  m <- NULL
  
  if(exists("cache_inverse_matrix") == TRUE && !is.null(cache_inverse_matrix)){
    message("solving cached matrix")
    m <- cache_inverse_matrix
    
  } else {
    message("cached matrix not found, continuing with the input")
    m <- solve(x)
    
  }
  m   
}