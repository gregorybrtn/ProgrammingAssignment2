
## These two functions are used to store a matrix and cache its inverse.

## Creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        my_inverse <- NULL
        set <- function(y) {
                x <- y
                my_inverse <<- NULL
        }
        get <- function() x
        inverse_setter <- function(inverse) my_inverse <<- inverse
        inverse_getter <- function() my_inverse
        list(set = set,
             get = get,
             setInverse = inverse_setter,
             getInverse = inverse_getter)
}


## Computes the inverse of the matrix created above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_inverse <- x$inverse_getter()
        if (!is.null(my_inverse)) {
                message("getting cached data")
                return(my_inverse)
        }
        mat <- x$get()
        my_inverse <- solve(mat, ...)
        x$inverse_setter(my_inverse)
        my_inverse
}
