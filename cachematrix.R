## This pair of functions is used to create an object that stores a
## numeric vector and caches its inverse.

## makeCacheMatrix:
##      Input: (invertible) matrix
##      Output: A list containing a function to 
##                      - set the value of the matrix
##                      - get the value of the matrix
##                      - set the value of the inverse of the matrix
##                      - get the value of the inverse of the matrix
##      Use: makeCacheMatrix creates a version of the input matrix which enables 
##           caching of its inverse

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) minv <<- solve
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve:
##      Input: output list from makeCacheMatrix consisting of function to
##                      - set the value of the matrix
##                      - get the value of the matrix
##                      - set the value of the inverse of the matrix
##                      - get the value of the inverse of the matrixtion
##      Output: inverse of matrix
##      Use: cacheSolve calculates and returns the inverse of the matrix if it 
##           has not been calculated already, or returns the cached value of the 
##           inverse if it has been previously calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv        
}
