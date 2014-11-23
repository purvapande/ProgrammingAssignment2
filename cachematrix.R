## Functions makeCacheMatrix and cacheSolve, create a special object that stores 
## a matrix and caches its inverse.

## makeCacheMatrix (x=matrix()) function defines functions to
## 1. Set the value of a matrix
## 2. Get the value of a matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function (invr) inv <<- invr
  getinv <- function () inv
  list (set = set, get=get,
        setinv = setinv, 
        getinv = getinv)
}


## CacheSolve calculates the inverse of a matrix. If the inverse has been cached, 
## it returns the value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve (data) 
  x$setinv(inv)
  inv
}
