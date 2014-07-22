
## Creates a special "matrix", which is really a list containing the following
## functions:
## set - sets the value of the matrix
## get - gets the value of the matrix
## set_inverse - set the value of the inverse
## get_inverse - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## Returns a matrix that is the inverse of the passed matrix.
## If the inverse has already been cached, it returns the cached matrix,
## otherwise the inverse is calculated, cashed, and returned
cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
