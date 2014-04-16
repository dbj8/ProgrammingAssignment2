## makeCacheMatrix and cacheSolve together provide for the creation of a matrix 
## whose inverse is automatically cached upon solving

## takes a matrix as input, provides getter/setter methods for contents and
## inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse,
                                   getinverse = getinverse)
}


## takes "special" matrix created from makeCacheMatrix() and both caches
## and returns it's inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
