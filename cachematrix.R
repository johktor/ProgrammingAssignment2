## The functions in this file will cache the inverse calculation of a matrix
## 

## makeCacheMatrix takes a matrix and converts it to a matrix that can be cached
## with associeted functions

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        setSolve <- function(solved) mat_inv <<- solved
        getSolve <- function() mat_inv
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## CacheSolve inverts the matrix given as an argument to makeCacheMatrix
## using a cached inverse if cacheSolve has been called before for the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m        
}
