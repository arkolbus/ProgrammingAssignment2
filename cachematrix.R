## Functions to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" that can cache its inverse
## Implementation is a list containing functions to:
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the matrix inverse
## 4. Get the matrix inverse

makeCacheMatrix <- function(cached_matrix = matrix()) {
    # Initialize the cached matrix inverse to NULL
    cached_matrix_inverse <- NULL
    
    # Function to set the value of the matrix
    # When the matrix is changed, the cached inverse to NULL
    set <- function(y) {
        cached_matrix <<- y
        cached_matrix_inverse <<- NULL
    }
    
    # Function to return the matrix
    get <- function() cached_matrix
    
    # Function to set the cached matrix inverse
    setinverse <- function(inverse) cached_matrix_inverse <<- inverse
    
    # Function to return the cached matrix inverse
    getinverse <- function() cached_matrix_inverse
    
    # Return value: list of functions for setting/getting the matrix/inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of a matrix.
## Matrix must be invertable
## Matrix must be created with the makeCacheMatrix function (above)
## If matrix inverse is cached, this function returns the cached inverse
## Otherwise, it calculates, caches, and returns the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## If the inverse is cached, return the cached inverse
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If we got here, the inverse is not cached
    
    # Get the matrix from the CacheMatrix object
    data <- x$get()
    
    # Calculate the matrix inverse
    inv <- solve(data, ...)
    
    # Cache the matrix inverse
    x$setinverse(inv)
    
    # Return the matrix inverse
    inv
}
