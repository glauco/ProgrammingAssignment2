## Assignment: Caching the Inverse of a Matrix

## Creates a cacheMatrix which consists of a list of functions that
## operates on a provided matrix.
##
## Usage example:
##
## Given the following matrix:
## > x <- matrix (c(1,2,3,4), nrow=2, ncol=2)
## When I execute this function:
## > cacheMatrix <- makeCacheMatrix(x)
## Then it returns a list of functions that operate on the matrix
## And I could get the matrix by executing:
## > cacheMatrix$get()
## And I could set a new matrix by executing:
## > cacheMatrix$set(matrix(c(4,3,2,1), nrow=2, ncol=2))
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        get <- function() { x }
        set <- function(newMatrix) {
                x <<- newMatrix
                inverse <<- NULL
        }
        getInverse <- function() { inverse }
        setInverse <- function(newInverse) {
                inverse <<- newInverse
        }
        list(get = get,
             set = set,
             getInverse = getInverse,
             setInverse = setInverse)
}

## Calculates and caches the inverse of a matrix.
##
## Usage example:
##
## Given the following cacheMatrix:
## > cacheMatrix <- makeCacheMatrix(matrix(c(4,3,2,1), nrow=2, ncol=2))
## When I execute this function:
## > cacheSolve(cacheMatrix)
## Then I get the inverse matrix:
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
## And I have now this inverse matrix cached on my cacheMatrix:
## > cacheMatrix$getInverse()
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
