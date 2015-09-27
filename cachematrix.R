## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix() - Stores the inverse in the cache if not already present
#                      (has helper functions)
# cacheSolve() - Return the inverse of the given matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Returns a list if inverse is present else it stores it in cache
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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
