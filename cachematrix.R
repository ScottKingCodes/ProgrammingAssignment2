## Matrix inversion can be a costly computation so to help mitigate that, the functions below
## cache the inverse of a matrix rather than compute it repeatedly.  These pair of functions
## cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversematrix <- function(solve) m <<- solve
    getinversematrix <- function() m
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calcualted (and the matrix has not changed), then the cache solve
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinversematrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinversematrix(m)
    m
}
