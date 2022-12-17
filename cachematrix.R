## These two functions should allow me to calculate and cache an inverse of
## matrix. The first function (makeCacheMatrix) generates the inverse and
## cache. The second uses the first function to either calculate the inverse
## or, if already cached, just call it.

## This function creates a "vector" of functions that generate the inverse
## matrix and store it in the environment.

makeCacheMatrix <- function(x = matrix()) {
    inv  <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## This function uses the "function" vector from the makeCacheMatrix function
## to return the inverse matrix, either using the cached one or calculating it.

cacheSolve <- function(x, ...) {
        invM <- x$getinv()
        if(!is.null(invM)) {
          message("Getting cached inverse")
          return (invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setinv(invM)
        invM
}

