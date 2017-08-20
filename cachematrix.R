## Caching the Inverse of a Matrix


## makeCacheMatrix will 1. Get & Set the matrix and 2. Get & Set the inverse 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_i <- function(i_mat) i <<- i_mat
        get_i <- function() i
        list(set = set, get = get,
             set_i = set_i,
             get_i = get_i)
}


## cacheSolve checks if inverse of the matrix has already been calculated. 
#if so it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_i()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_i(i)
        i
}
