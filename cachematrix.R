## Per Assignmenet 2 Instructions:
## First makeCacheMatrix creates a special matrix object;
## then cacheSolve calculates the inverse of the matrix.
## If the inverse of the matrix has already been calculated, it will instead 
## locate it in the cache and return it, and not calculate it again,
## potentially saving a lot of computation time.
## These scripts assume the inverse of the matrix can be calculated.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function returns the inverse of a matrix A created with the
## previous makeCacheMatrix function.
## If the cached inverse is available --> cacheSolve retrieves it;
## if not --> computes, caches, returns inverse.
## Again has the potential to save a lot of computation time if the inverse
## matrix was previously calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinverse(inv_x)
                return(inv_x)
        }
}