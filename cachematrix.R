## This is a set of functions to help with matrix inversion and to boot will implement caching for efficiency
## function makeCacheMatrix: 
##			this function will create a matrix and retrieve from cache if possible
## function cacheSolve:
##			this function will handle matrix inversion and retrieve the inverted matrix from cache, if possible
##

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
