## These functions cache allow to run Solve on a matrix,
## If said matrix has already been solved, the result is fetched
## from the cache

## Creates an cache object intented for a matrix solve
## Has access and set methods
## Returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## tries to retrieve a matrix from the passed cache object
## if no solve exists, it runs the solve on said object
## Once solve is complete, results are cached to the cache object
## Returns a solved matrix
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
