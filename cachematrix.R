## This script caches the inverse of a matrix

## makeCacheMatrix creates a list of functions that set and get a matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y){
                x <<- y 
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix.
## If the inverse has already been calculated,cacheSolve gets it from the cache. 
## Otherwise, it calculates the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
        
        inv <-x$getinv()
        if(!is.null(inv)){
                message('Getting cahced data!')
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
