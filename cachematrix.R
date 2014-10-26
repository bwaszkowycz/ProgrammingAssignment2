## Solves and caches the inverse of a matrix
## or retrieves the inverse from cache if already stored  
## 

## makeCacheMatrix generates a list of functions to handle initialization (set*)
## and retrieval (get*) of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                # initialize variable that will store matrix_inverse
        set <- function(y) {     
                x <<- y          # assign to containing environment using <<- operator
                m <<- NULL       # reset to NULL on calling set()
        }
        get <- function() x      # retrieves matrix
        setInverse <- function(matrix_inverse) m <<- matrix_inverse # assigns inverse
        getInverse <- function() m     # retrieves inverse
        list(set = set, get = get,     
             setInverse = setInverse,
             getInverse = getInverse)  # list of functions returned
        
}

## cacheSolve returns the inverse of a matrix from cache 
## or generates and caches the inverse if no cache has been defined

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)) {          # check if cache exists
                message("getting cached data")
                return(m)          # return previously cached data
        }
        matrix <- x$get()          # else retrieve matrix and solve for inverse
        m <- solve(matrix, ...)
        x$setInverse(m)            # save inverse in containing environment 
        m
}
