## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) {                               ## set the value of the matrix 
                x <<- y
                inv <<- NULL
        }
        get <- function() x                                ## get the value of the matrix
        setInverse <- function(inverse) inv <<- inverse    ## set the value of the inverse
        getInverse <- function() inv                       ## get the value of the inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Write a short comment describing this function
## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
                                                          ## Return a matrix that is the
        inv <- x$getInverse()                              ## inverse of "x"
        if (!is.null(inv)) {
                message("getting cached data")             ## The operation is displayed
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
     
}
