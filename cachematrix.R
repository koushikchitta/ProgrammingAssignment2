## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Special matrix which can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setmatInverse <- function(i) mi <<- i
    getmatInverse <- function() mi
    list(set = set, get = get,
         setmatInverse = setmatInverse,
         getmatInverse = getmatInverse)
}

## Write a short comment describing this function
## cachesolve to get inverse of a matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatInverse(m)
    m
}
