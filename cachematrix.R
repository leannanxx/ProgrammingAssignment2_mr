## Put comments here that give an overall description of what your
## functions do

## the function makeCacheMatrix creates a matrix object which 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## the function cacheSolve computes the inverse of a matrix using the cache 
## if available

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
         if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
         }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
