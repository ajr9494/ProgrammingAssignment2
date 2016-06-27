## These two functions allow the user to calculate the inverse of an invertible matrix and
## to save any previously computed matrices into the cache, in order to aid with faster
## calculations.

## This function creates a list that sets the value of the matrix, gets the value of the
## matrix, sets the value of its inverse, and gets the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() s
        list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
}


## This function takes makeCacheMatrix() as its input. It uses this to either solve for the
## inverse of the matrix or find the already computed inverse in the cache. 

cacheSolve <- function(x, ...) {
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setInverse(s)
        s
}
