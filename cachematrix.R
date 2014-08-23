## The makeCacheMatrix function returns a list object with 4 matrix functions that allow
## users to cache the inverse of a matrix.

## Return 4 matrix functions in a list. This function assumes the input of an invertible
## matrix as indicated in the project description.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    print(x)
    get <- function() x
    setsolve <- function(solve){m <<- solve(x)}
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Returns the inverse of a matrix from the cache if it has already been caculated.
## Otherwise the function stores the inverse and returns the inverse function.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
