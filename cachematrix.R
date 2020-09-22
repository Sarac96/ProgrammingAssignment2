## Computing the cache of a matrix can be a costly operation.
## This function creates a matrix whose inverse can be retrieved from a cache.

## makeCacheMatrix creates "special" matrix, which is a list containing:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv_matrix <<- inverse
    getinverse <- function() inv_matrix
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## cacheSolve computes the inverse of the matrix created through makeCacheMatrix.
## If the inverse was already computed, it only retrieves the cached value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getinverse()
    if (!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data)
    x$setinverse(inv_matrix)
    inv_matrix
}