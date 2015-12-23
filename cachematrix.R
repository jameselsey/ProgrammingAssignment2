## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Creates a cached inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    # get cached matrix
    get <- function() x
    # set cached matrix
    set <- function(y) {
        x <<- y
        # matrix is now assigned so we can clear inverseMatrix
        inverseMatrix <<- NULL
    }
    
    # get cached value
    getinverse <- function() inverseMatrix
    # set cached value
    setinverse <- function(inverse) inverseMatrix <<- inverse
    
    # Last line returns a list, whereby each named parameter is a function as described above
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# returns the cached inverse matrix if available, othewise calculates, sets in cache, then returns it

cacheSolve <- function(x, ...) {
    # get the cached inverseMatrix
    inverseMatrix <- x$getinverse()
    # if inverseMatrix is not null, print message and return it
    if(!is.null(inverseMatrix)) {
        message("Retrieving cached inversed matrix")
        return(inverseMatrix)
    }
    # cached inverseMatrix was null, so attempt to solve and set it
    data <- x$get()
    inverseMatrix <- solve(data)
    x$setinverse(inverseMatrix)
    # return inverse matrix
    inverseMatrix
}
