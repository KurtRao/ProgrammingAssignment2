# This library encapsulates a matrix object with cached solve.
# To use this library, caller should store a matrix in a list
# returned by cached_matrix <- makCacheMatrix(the_matrix).
# After that, the caller should access the solve of the
# matrix by calling cacheSolve(cached_matrix).
# To access the matrix itself, the caller should use
# cached_matrix$get. To modify the matrix, the caller should
# use cached_matrix$set.


# This function will create a list contains the matrix and four
# methods which are respectively set(), get(), setsolve() and
# getsolve().
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


# this function first checks if the cache already exists.
# if it doesn't, the function will calculate the
# solve and then store it in cache. Next time the
# function gets called, it will return the cache
# directly.
# please note that, the input x is not the actual matrix
# but the list returned by mackCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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

