## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## m - inverse matrix value
        m <- NULL
        ## set: matrix and reset m
        set <- function(y) {
        ## set the value of the matrix and clear the value of inverse matrix
        ## The "<<-" operator used to set variable that already exists 
        ## in the parent environment.
                x <<- y
                m <<- NULL
        }
        ## get: get matrix
        get <- function() x
        ## Set inverse matrix
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Check if it is not null
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If it is null, calculate the inverse
        data <- x$get() # get the matrix to inverse
        m <- solve(data, ...) # inverse matrix
        x$setinverse(m) # store the result in makeCacheMatrix
        m # return inverse matrix
}
