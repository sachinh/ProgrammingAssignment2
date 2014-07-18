## This is a set of functions to help with matrix inversion and to boot will implement caching for efficiency
## function makeCacheMatrix: 
##			this function will create a matrix and retrieve from cache if possible
## function cacheSolve:
##			this function will handle matrix inversion and retrieve the inverted matrix from cache, if possible
##

## Declare String Literals for messages and such
## Can help with Localization needs, if required
s_Square_Matrix_Allowed = "Inverting of non-square matrices is not allowed in this function."
s_Cached_Matrix = "Getting cached data"

## this function stores a cached matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        # Do the error check for non-Square Matrix
        if ( !isSquareMatrix(x) ){
                message(s_Square_Matrix_Allowed)
                return (matrix(0))
        }
        
        ## this function will allow to change the matrix after instantion
        set <- function(y) {
                
                # Do the error check for non-Square Matrix
                if ( !isSquareMatrix(y) ){
                        message(s_Square_Matrix_Allowed) 
                        return ()
                }
                
                # Valid matrix so go ahead with the 'setting' of data
                x <<- y
                m <<- NULL
        }
        
        # other functions: get, setsolve & getsolve are trivial
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

## this function returns the inverted matrix if its available.
## if not, then inverts the stored matrix and caches this matrix.

cacheSolve <- function(x, ...) {
        
        ## get the inverted matrix 
	m <- x$getsolve()
        
        ## if the inverted matrix exists, then return this matrix with a message
        if(!is.null(m)) {
                message(s_Cached_Matrix)
                return(m)
        }
        
        ## this means there is no stored inverted matrix so invert accordingly
        data <- x$get()
        
        ## error check for non-square matrix
        if (!isSquareMatrix(data)) {
                message(s_Square_Matrix_Allowed)
                return (matrix())  
        }
        
        ## now invert the matrix and return
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## Utility Function to check for a square matrix
isSquareMatrix <- function(mat = matrix()) {
        ## now extract the number of rows and columns
        num_rows <- nrow(mat)
        num_cols <- ncol(mat)
        
        # now check if rows are same as cols i.e. square matrix
        matrix_is_square <- FALSE
        if (num_rows == num_cols)
                matrix_is_square <- TRUE
        
        matrix_is_square
        
}