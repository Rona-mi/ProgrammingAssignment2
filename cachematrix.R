## This function creates a special 'matrix' object that can cache its inverse.  it first checks to see if the matrix inversion
## has already been calculated.  If so, it gets the matrix inversion from the cache and skips the computation.  Otherwise, it
## calculates the matrix inversion and sets the value of the new matrix in the cache via the setCalculated function.
makeCacheMatrix <- function(original = matrix()) {
    stored <- NULL
    get <- function() {
        return(original)
    }
    setCalculated <- function(justCalculated) {
        stored <<- justCalculated
    }
    getCalculated <- function() {
        return(stored)
    }
    
    list(get = get, setCalculated = setCalculated, getCalculated = getCalculated)
}

## This function computes the inverse of the special 'matrix' returned by makeCacheMatrix above.  If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getCalculated()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setCalculated(m)
    m
} 
