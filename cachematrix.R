makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # Initialize the cached inverse as NULL

    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL  # Clear the cached inverse when the matrix is set
    }

    # Get the value of the matrix
    get <- function() x
    
    # Set the cached inverse
    setinverse <- function(inverse) m <<- inverse
    
    # Get the cached inverse
    getinverse <- function() m
    
    # Return a list of functions to interact with the matrix object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
    m <- x$getinverse()  # Get the cached inverse
    if(!is.null(m)) {
        message("getting cached data")  # If inverse is cached, return it
        return(m)
    }
    
    # If the inverse isn't cached, calculate it
    data <- x$get()  # Get the matrix
    m <- solve(data, ...)  # Compute the inverse using solve()
    x$setinverse(m)  # Cache the inverse for future use
    
    return(m)  # Return the computed inverse
}
