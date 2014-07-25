## Matrix inversion caching script 
## a pair of functions are created to cache and retrieve inverse matrices

## makeCacheMatrix: creates "matrix" object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(x) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## cacheSolve: computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix. If the inverse has already 
##      been calculated then cacheSolve retrieves the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) { 
                message('getting cached matrix')
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
