## makeCacheMatrix: creates a special matrix that can cache its inverse.
## cachSolve: function computes the inverse of the special matrix returned by
## mackCacheMatric

## Create a special matrix, a list to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matric

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
                setinverse = setinverse, 
                getinverse = getinverse)
}


## Compute the inverse of the matrix created with the makeCacheMatrix function
## 1. get the inverse from cache
## 2. if the inverse exists i.e not NULL return cache value otherwise
##    compute the inverse, store it in the cache and return the inverse
## Warning if the matrix is not square this function will fail with an error

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


