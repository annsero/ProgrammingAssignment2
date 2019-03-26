## A pair of functions that cache the inverse of a matrix.

## The following function creates a special matrix object that stores a matrix 
## and its inverse. It builds a list of functions to set the matrix, get the 
## matrix, set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y    
                i <<- NULL   
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function returns the inverse of the matrix. It first checks if 
## the inverse has already been calculated for the current matrix in which case 
## it retrieves it from the cache. If not it computes the inverse and sets the 
## value in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
## If the inverse of the matrix has already been calculated return the inverse 
## value from the cache.
        if(!is.null(i)) {    
                message("getting cached data")
                return(i)
        }
## Otherwise, calculate the inverse and set the value in the cache.
        data <- x$get()
        i <- solve (data, ...)
        x$setinverse(i)    
        i
}
