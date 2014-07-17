# The two functions below interact to decrease redundant inverse calculations.
# Instead of passing around matrices, makeCacheMatrix creates a cachelist with
# room for inputmatrix and its inverse. cacheSolve checks if the inverse
# is present in the cachelist, else solves for it and stores it in the cachelist. 

# makeCacheMatrix can be thought of as a constructor, and places a matrix 
# in a list, with place for its inverse. 
makeCacheMatrix <- function(X = matrix()) {
        
        # Clear M
        M <- NULL
        
        # Assign inputdata in .GlobalEnv as X, clear M
        set <- function(y) {
                X <<- y
                M <<- NULL
        }
        
        # Get inputdata
        get <- function() X
        
        # Cache inverse
        setinv <- function(inv) M <<- inv
        
        # Retrieve cached inverse
        getinv <- function() M
        
        # Put the pieces into a list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#CacheSolve takes the list generated from makeCacheMatrix and
# calculates (if not already done) the inverse of the matrix
# sent as input to makeCacheMatrix. The calculated inverse 
# is stored in the list from makeCacheMatrix.

cacheSolve <- function(X, ...) {
        # Check for cached value
        M <- X$getinv()
        if(!is.null(M)) {
                
                # If present, return the cached inverse.
                
                message("getting cached data")
                return(M)
        }
        
        # Else, get the data from makeCacheMatrix
        data <- X$get()
        # and solve for inverse
        M <- solve(data, ...)
        # save in cache
        X$setinv(M)
        # and output the inverse.
        M
}
