## Use cacheSolve() to perform the matrix inverse. It will return a cached
## solution if previously done. makeCacheMatrix() has the internals get/set
## methods and can compute solve

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    d <- x$get()
    i <- solve(d, ...)
    x$setinv(i)
    i
}
