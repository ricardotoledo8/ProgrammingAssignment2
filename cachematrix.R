## makeCacheMatrix returns a list containing functions to:
## (1) set -> Set the Matrix
## (2) get -> Get the Matrix
## (3) setinverse -> Set the Inverse
## (4) getinverse -> Get the Inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## (1)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## the operator <<- assign the value of an object in an environmet
        ## different from the current environment
        
        ## (2)
        get <- function() x
        
        ## (3)
        setinverse <- function(inverse) m <<- inverse
        
        ## (4)
        getinverse <- function() m
        
        ## returns the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve returns the inverse of the original matrix in makeCacheMatrix
## the function cacheSolve uses the list of functions above

cacheSolve <- function(x, ...) {
        ## 'x' is the output of makeCacheMatrix
        m <- x$getinverse()
        
        ## if the inverse has been calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## otherwise, calculates the inverse
        data <- x$get()
        m <- solve(data, ...)
        
        ## sets the inverse in the cache
        x$setinverse(m)
        m
}