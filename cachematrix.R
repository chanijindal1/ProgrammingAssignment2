## Program takes an invertible matrix as an input and stores the reverse in a cache
## to save the repetitive computation.

library(matrixcalc)

## Caches the reverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  reverse <- NULL
  set_matrix <- function(y) {
    x <<- y
    reverse <<- NULL
  }
  get_matrix<- function() x
  set_matrix_reverse <- function(solve) reverse <<- solve
  get_matrix_reverse <- function() reverse
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_matrix_reverse = set_matrix_reverse,
       get_matrix_reverse = get_matrix_reverse)
}


## Returns a matrix that is an inverse of the given matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  reverse <- x$get_matrix_reverse()
  if(!is.null(reverse)) {
    message("getting cached matrix data")
    return(reverse)
  }
  mat <- x$get_matrix()
  
  ## Exit if the matrix is singular/non-invertible
  if(is.singular.matrix(mat)) {
    message("Matrix is non-invertible/singular")
    return()
  }
  
  reverse <- solve(mat, ...)
  x$set_matrix_reverse(reverse)
  reverse
}
