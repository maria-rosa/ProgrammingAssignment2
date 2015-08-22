## This file contains two functions that allow to cache the inverse of 
## a matrix once it is calculaterd so it can be used later on.

## The makeCacheMatrix fuction creates an object, using argument matrix x.
## it returns a list containing four functions:
## - set: to set the matrix x. When called by given matric y, it sets y y as the
##        "new" x in the makeCacheMatrix environment and clears any previously 
##        calculated inverse value (which would be incorrect for the new matrix)
## - get: to get the matrix v from the object created by makeCacheMatrix
## - setinv: to store the inverse of matrix x
## - getinv: to retrieve previouly stored inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)

}


## The cacheSolve function takes an object created by the makeCacheMatrix function
## as argument. If this object has a previouly stored inverse, it returns this
## matrix. Otherwise, it calculates the inverse and stores it in object x.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     message("calculating inverse")
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
