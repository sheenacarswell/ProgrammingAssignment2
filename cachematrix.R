## Functions for caching the inverse of a matrix (using solve()), to avoid calculating it more than once
##
## Example usage:
## > m <- matrix(c(20, 0.5, 4, 9), nrow = 2, ncol = 2)
## > mycachematrix = makeCacheMatrix(m)
## > mycachematrix$get()
##      [,1] [,2]
## [1,] 20.0    4
## [2,]  0.5    9
## > cacheSolve(mycachematrix)
##              [,1]        [,2]
## [1,]  0.050561798 -0.02247191
## [2,] -0.002808989  0.11235955
## > cacheSolve(mycachematrix)
## getting cached data
##              [,1]        [,2]
## [1,]  0.050561798 -0.02247191
## [2,] -0.002808989  0.11235955
## > 

## Creates a special matrix, with functions to allow getting and setting its value and its inverse
makeCacheMatrix <- function(x = matrix()) {
	storedInverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        storedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) storedInverse <<- inverse
    getInverse <- function() storedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Gets the matrix inverse, either freshly calculated, or from cache if it has been calculated already
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    storedInverse <- x$getInverse()
    if(!is.null(storedInverse)) {
        message("getting cached data")
        return(storedInverse)
    }
    data <- x$get()
    newInverse <- solve(data, ...)
    x$setInverse(newInverse)
    newInverse
}
