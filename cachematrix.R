## The created functions do:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve:      This function computes the inverse of the special "matrix" returned by 
##                  makeCacheMatrix above. If the inverse has already been calculated 
##                  (and the matrix has not changed), then the cachesolve should retrieve 
##                  the inverse from the cache.

## The function makeCacheMatrix will return a list of 4 functions: 
## - the first will set the value of the matrix 
## - the second will get the value of the matrix 
## - the third will set the value of the inverse 
## - the fourth will get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) { 
## the value inv will store the cached inverse matrix 
      inv <- NULL 
      
## will set the matrix 
      set <- function(y) { 
               x <<- y 
               inv <<- NULL 
      } 
## Will get the matrix 
      get <- function() x 
## Will set the inverse 
      setinv <- function(inverse) inv <<- inverse 
## Will get the inverse 
      getinv <- function() inv
## Will return the matrix produced with our newly defined functions 
     list(set = set, get = get, setinv = setinv, getinv = getinv) 
} 

## The function cacheSolve will compute the inverse of the matrix. If the inverse
## is already calculated before, it returns the cached inverse. 

cacheSolve <- function(x, ...) { 
## loads inverse into inv if inverse was calculated before     
      inv <- x$getinverse() 
## Checks if inverse was calculated before, If the inverse is already calculated, it is returned      
      if(!is.null(inv)) { 
         message("getting cached data.") 
         return(inv) 
     } 
## If the inverse is not yet calculated, it will be calculated     
      data <- x$get() 
      inv <- solve(data) 
## caches the calculated inverse      
      x$setinverse(inv) 
## shows the calculated inverse
      inv 
} 