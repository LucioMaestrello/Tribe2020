## Write functions that can cache the inverse of the target matrix

## makeCacheMatrix <- a function that creates an object that caches 
## the inverse of the invertible square matrix/input desired

rm(list = ls())
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  s.inverse <- function(inverse) inv <<- inverse
  g.inverse <- function() inv
  list(set = set,
       get = get,
       s.inverse = s.inverse,
       g.inverse = g.inverse)
}

## The function below computes the inverse of the matrix created by the
## makeCachematrix above. Since inverse is calculated, the inverse from 
## thr cache will be retrieved.

## Returns matrix that is inverse of x

cacheSolve <- function(x, ...) {
  inv <- x$g.inverse()
  if (!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$s.inverse(inv)
  inv
}

## CHECK PROGRAM ##
tribe_matrix <- makeCacheMatrix(matrix(1:4, 2,2))
tribe_matrix$get()
tribe_matrix$g.inverse()
cacheSolve(tribe_matrix)
tribe_matrix$g.inverse()
tribe_matrix$set(matrix(c(1,2,2,3), 2, 2))
tribe_matrix$get()
tribe_matrix$g.inverse()
cacheSolve(tribe_matrix)
tribe_matrix$g.inverse()
