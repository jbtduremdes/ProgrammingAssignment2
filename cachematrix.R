## The following functions computes the inverse of a matrix and
## stores it in cache. It avoids repeated computing by retrieving
## the result for an already calculated matrix inversion in cache.

## The function below creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getInverse <- function() inverse
    list(set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## The function below computes the inverse of the special "matrix" 
## returned by 'makeCacheMatrix'. If the inverse has already been
## calculated (and the matrix has not changed), 'cacheSolve'
## retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Retrieve cached data...")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
