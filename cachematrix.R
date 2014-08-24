## cache the inverse of a matrix
## create a special object that stores a matrix and cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# check to see if the inverse has already been calculated
# if yes, get the cached data and skip the computation
cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}