## The following two functions are used to create a special object, 
##which stores a matrix and caches its inverse.


## This first function makeCacheMatrix creates a list, 
## which contains functions to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(z) inv <<- z
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## This second function returns a matrix which is the inverse of 'x' 
## either by retrieving it from the cache or (if it has not yet 
## been stored to the cache) by computing the inverse using 'solve'. 
## This function takes the object returned by makeCacheMatrix as its input.

cacheSolve <- function(x, ...) {
          inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

