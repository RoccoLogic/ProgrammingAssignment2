## this function makes a list of four functions to be used for caching the matrix
## set the matrix, get the matrix, set the inverse of the matrix, get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
}
        get = function() x
        setinverse = function(inverse) inv <<- inverse 
        getinverse = function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## this function calculates the inverse of the matrix
## or if the inverse has already been set it retrieves it from the cache

cacheSolve <- function(x, ...) {
        inv = x$getinverse()
        if (!is.null(inv)){
                message("getting data from cache")
                return(inv)
        }
        
        matrix = x$get()
        inv = solve(matrix, ...)
        
        x$setinverse(inv)
        
        return(inv)
}
