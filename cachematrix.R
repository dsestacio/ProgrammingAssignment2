## Put comments here that give an overall description of what your
## functions do

## Function to create a matrix object (set)
## - can cache the inverse of the matrix (setInverse)
makeCacheMatrix <- function(x = matrix()) {
        matrixCache <- NULL
        set <- function(y) {
            x <<- y
            matrixCache <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrixCache <<- inverse
        getInverse <- function() matrixCache
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve accepts a matrix object (x)
## checks if the inverse of the matrix has been cached
##      if already cached, returns the cached matrix inverse
##      if not cached, solves the inverse and caches the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
