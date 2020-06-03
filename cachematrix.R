## Assignment 2 - RProgramming course Víctor Camarena June 2020


## Pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) m <<- inverse
        getinverse <-  function() m
        list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}


##cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## In case the inverse has already been calculated, the cacheSolve retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
