## The two functions below, makeCacheMatrix and cacheSolve, when used together 
## provide a caching for the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x) {
    m <- NULL
    im <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        im <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    setinverse <- function(matrixinverse) im <<- matrixinverse
    getinverse <- function() im
    list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}