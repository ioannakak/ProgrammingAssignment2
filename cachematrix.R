# Peer Graded Assignment: Programming Assignment 2: Caching the Inverse of a Matrix
## The first function is called "makeCacheMatrix" and
## creates a  "matrix" object and then cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## x  is a square invertible matrix
  ## and will be return: a list containing functions to
  ##               set the matrix
  ##               get the matrix
  ##               set the inverse
  ##               get the inverse
  ##         and pass the list in input to cacheSolve()
  
  
  environment = NULL 
  set <- function(y) { 
    x <<- y
    environment <<- NULL
  } 
  get <- function()x 
  setinverse <- function(inverse) environment <<- inverse 
  getinverse <- function() environment 
  list(set = set,get=get, getinverse = getinverse, setinverse = setinverse)
  
}

## cacheSolve()is a function which calculate the inverse of the "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated the function retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  ## x: output of makeCacheMatrix() in our first function.
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inverse = x$getinverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  } 
  
  matrix.data = x$get() 
  inverse = solve(matrix.data, ...) 
  x$setinverse(inverse) return(inverse) }
