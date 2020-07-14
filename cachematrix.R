##Caching the Inverse of a Matrix.
##Matrix inversion is a costly computation therefore it is useful to be able 
##to store the inverse of a matrix in the cache. The functions below
##allow the user to create a matrix that can be stored in cache, invert 
##the matrix, and recall the inverse of the matrix from the cache
##if the inverse was calculated previously. 

##This function creates a special "matrix" object that can cache/store its 
##inverse.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" object that was
##created by the makeCacheMatrix function above. If the inverse has already
##been computed and assuming that the special "matrix" object has 
##not been edited, the function will recover the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}