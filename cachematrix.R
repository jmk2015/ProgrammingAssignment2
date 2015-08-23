## These functions demonstrate lexical scoping

## makeCacheMatrix receives a matrix (assumed to be square/invertible)
## and returns matrix data and internal functions.
## cacheSolve will return the inverted matrix (either cached from previous
## or it will call the solve function to perform.)

makeCacheMatrix <- function(x = matrix()) {
    
    inv_matrix <- NULL                          # initialize
    
    get <- function() x                 # called when matrix has never been
                                        # inverted. Returns the matrix
    
    setsolve <- function(result) {      # called after matrix inverted
        inv_matrix <<- result           # caching the result
    }
    
    getsolve <- function() inv_matrix   # always called. 
                                        # inv_matrix may or may not be NULL
    
    list( get = get, 
          setsolve = setsolve,
          getsolve = getsolve )         #return functions
    
}


## This function will return the inverse of a matrix (assumed to be invertible)
## If the matrix has previously been inverted, it returns the cached inverse.
## Otherwise, it calls the solve function to invert, and puts it into the 
## cache variable

cacheSolve <- function(x, ...) {
    
    
    inv_matrix <- x$getsolve()      # get what is hoped to be cached invert
    
    # check to see if it was previously inverted and cached
    if( !is.null(inv_matrix)) {         # it was cached. return that
      #  message("getting cached data")  # (leave for debugging purposes)
        return(inv_matrix)              
    }
    
    # if we get here, the inverse was never performed and cached.
    data <- x$get()                     # get the matrix
    inv_matrix <- solve(data,...)       # invert it
    x$setsolve(inv_matrix)              # NOTE: set cache
    inv_matrix                          
    
}
