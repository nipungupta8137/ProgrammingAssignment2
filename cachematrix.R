## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a matrix. Following functions are available:--
# set and get new matrix values
# setinverse and getinverse value matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invMatrix <<- inverse
        getinverse <- function() invMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
# cacheSolve checks if the cache matrix has inverse if not it calculate the inverse and call back to store in the cache matirx.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getinverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
	  # inverse matrix is not cached so computing at this point
        invMatrix <- solve(data)
	  # sending valur to be set to the cache matrix created earlier
        x$setinverse(invMatrix)
        invMatrix
}
