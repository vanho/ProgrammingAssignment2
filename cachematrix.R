## makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse, including:
# set the value of the matrix
# get the value of the vatrix
# set the inverse of the matrix
# get the inverse of the matrix.
# The matrix is invertible.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL 
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the inverse of the matrix using solve
    setinverse <- function(solve) i <<- solve
    
    # Get the inverse of the matrix
    getinverse <- function() i
    
    list(set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
    
}


# cacheSolve computes the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve retrieves the inverse from 
# the cache.
cacheSolve <- function(x, ...) {
    # Retrieve the inverse for x
    i <- x$getinverse()
    if (!is.null(i)) {
        # The inverse has already been calcuated, return the cache
        message("getting cached data")
        return(i)
    }
    
    # Inverse has not been calcuated, calculate it now
    data <- x$get()
    i <- solve(data, ...)
    
    # Set calculated inverse 
    x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}

