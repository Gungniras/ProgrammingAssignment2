## The 2 following functions work together to create and store the inverse of a matrix
## If the inverse has been created before of this matrix, it is not recomputed but 
## copied from the cache

## This function stores the inverse of a matrix if computed before

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvertedmatrix <- function(solve) m <<- solve
    getinvertedmatrix <- function() m
    list(set = set, get = get,
         setinvertedmatrix = setinvertedmatrix,
         getinvertedmatrix = getinvertedmatrix)
}


## This function first checks if the inverse of the matrix has been computed before
## if so it gets the inverse from the cache and prints it, if not it calculates and prints it

cacheSolve <- function(x, ...) {
    m <- x$getinvertedmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvertedmatrix(m)
    m
}





