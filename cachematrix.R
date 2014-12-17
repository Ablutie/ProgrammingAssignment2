# These functions create an object which stores a matrix and then
# calculate the inverse of the matrix


# This function creates and stoes a matrix and its inverse in 
# the cache and defines operations for setting and retrieving
# this information

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y=matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function checks whether the inverse of the previously created
# matrix is in the cache; if so, it retrieves it, else it calculates
# the inverse and stores it

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                print("retrieving cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
