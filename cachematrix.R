## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##function sets the value of the matrix
## gets the value of the matrix
## sets the value of the inversed matrix 
##gets the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initially nothing is cached so set it to NULL
        inv <- NULL
        ## store a matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## returns the stored matrix
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        ## get the cached value
        getInverse <- function() inv
        ## return a list
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## get the cached value
        inv <- x$getInverse()
        ## if a cached value exists return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## otherwise get the matrix, caclulate the inverse and store it in the inv
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        ## return the inverse
        inv
}
