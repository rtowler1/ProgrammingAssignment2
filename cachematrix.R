## makeCacheMatrix and cacheSolve are used together
## makeCacheMatrix stores the inverse of a matrix
## along with the matrix.
##
## cacheSolve will return the inverse of the matrix
## from the cache if it exists and the matrix has not changed
## otherwise it uses solve to find the inverse and stores it
## for future calls.
## 

## Create a matrix object that caches the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(solve) im <<- solve
        getinv <- function() im
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## return the inverse of the matrix from the cache if possible
## if it does not exist compute inverse cache & return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinv()
        if (!is.null(im) && 
            dim(im) == dim(x$get()) &&
            im == x$get()
            ) {
                message("returning cached inverse matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im
}
