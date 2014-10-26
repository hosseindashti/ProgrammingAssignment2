##assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix() ) {
    m <- NULL
    ## set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the matrix
    get <- function() {
        x
    }
    ## set the inverse of matrix
    setinverse <- function(inverse) {
        m <<- inverse
    }
    ## get the inverse of matrix
    getinverse <- function() {
        m
    }
    ## return a list
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## if the mean has already been calculated then gets the mean from the cache and skips the computation
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    ## get the matrix x 
    data <- x$get()
    ## calculate inverse of matrix x
    m <- solve(data, ...)
    ## set the inverse of matrix x
    x$setinverse(m)
    ##return values
    m
}
