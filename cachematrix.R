## Assignment 2: Lexical scoping
## by Kanin Tarntira
## The two functions below can be used to calculate the inverse of any
## invertible input matrices while collect their cache to reduce the
## time consumed by the next inverse on the same matrix again.

## The first function creates a special matrix that is able to store
## and call cache from the global environment

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_input) inv <<- inv_input
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function first check if the result is already collected
## as a cache or not, if yes then it will return the cache, if not then
## the result is needed to recalculate again

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data_mat <- x$get()
        inv <- solve(data_mat, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
