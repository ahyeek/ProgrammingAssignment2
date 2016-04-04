## 2 functions : 
## Given a invertible matrix, the following two functions will calculate the inverse matrix
## or retrieve the inverse matrix from the cache.

## This is the function that created a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) m <<- solve
    
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function that compute the inverse of special 'matrix' resolve and return by makeChcheMatrix.
## If the inverse has already been calculated (and matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.
## Not performing the calculation once more.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setinverse(m)
    
    m
}