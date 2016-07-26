## Calculates inverse of matrix. Uses cached mean if inverse is already computed.

## Creates a matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

## Calculates the inverse of the matrix. If mean is calculated it gets the mean from cache and skips computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("getting cashed data")
        return(i)
    }
    i <- solve(x$get(), ...)
    x$set_inverse(i)
    i
}


