## Caching the Inverse of a Matrix

## This package adds two functions:

## - makeCacheMatrix() : to create an object (vector of 4 functions to calculate and store the inverse of a matrix)
## - cacheSolve() : to check if an inverse of a matrix exists, and if not, calculate and cache the result

## Example of usageL

## > x <- matrix( c(4,3,3,2), 2,2)

## > m <- makeCacheMatrix(x)

## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## makeCacheMatrix(x) - where x is a square matrix that can be inverted
##    This function does not check if inversion is possible. 
##    It is assumed only invertable matrices will be used at this stage.

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) { 
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) sol <<- solve
        getsolve <- function() sol
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve(m) - where m is a vector of 4 functions, calculated with makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## sol is a matrix that is the inverse of 'x'
        sol <- x$getsolve()  
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsolve(sol)
        sol
}
