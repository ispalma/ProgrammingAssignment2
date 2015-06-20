## The two following fuctions will cache and compute the inverse of a given matrix


## The first one creates  a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set<- function (y){
                x <<- y;
                inverse <<- NULL
        }
        get <- function() return(x);
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() return(inverse)
        return (list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))        
}


## This second function computes the inverse of the matrix created by the former function above from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("Getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        return(inverse)
}

