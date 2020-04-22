Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly (there are also alternatives to matrix inversion that we will
            not discuss here). Your assignment is to write a pair of functions that
cache the inverse of a matrix.

Write the following functions:
    
    1.  `makeCacheMatrix`: This function creates a special "matrix" object
that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
"matrix" returned by `makeCacheMatrix` above. If the inverse has
already been calculated (and the matrix has not changed), then
`cacheSolve` should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the `solve`
function in R. For example, if `X` is a square invertible matrix, then
`solve(X)` returns its inverse.

For this assignment, assume that the matrix supplied is always
invertible.



## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'

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


# test area

testmatrix <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)



