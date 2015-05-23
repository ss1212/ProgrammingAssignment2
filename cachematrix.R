# makeCacheMatrix(matrix)
# This function creates a special "matrix" object that can cache its inverse.
# This function does not compute the inverse of the matrix. It merely saves the
# provided matrix or inverse. 
# Object retruned by this function is a list of following methods

# setMatrix the matrix to cache
# getMatrix get cached matrix
# setInverse cache inverse of the matrix
# getInverse get cached inverse of the matrix

makeCacheMatrix <- function(cachedMatrix = matrix()) {
        # Initialize inverse to null
        cachedInverse <- NULL
        setMatrix <- function(m) {
            # Set matrix
            cachedMatrix <<- m
            # Initialize inverse to null
            cachedInverse <<- NULL
        }
        getMatrix <- function() {
            # retrun cached matrix
            cachedMatrix
        }
        setInverse <- function(inverse) {
            # cache inverse
            cachedInverse <<- inverse
        }
        getInverse <- function() {
            # return cached inverse
            cachedInverse
        }
        # list of methods available to read/cache
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


# cacheSolve(cachedMatrix)
# This function accepts the object returned by makeCacheMatrix.
# computes and saves inverse of matrix in the supplied objectcm

cacheSolve <- function(cacheMatrix, ...) {
    # get cached inverse
    inverse <- cacheMatrix$getInverse()
    # if inverse is already cached, return it. no need to compute again
    if(!is.null(inverse)) {
        return(inverse)
    }
    # get cahced matrix
    m <- cacheMatrix$getMatrix()
    # compute inverse
    inverse <- solve(m, ...)
    # cache invrese in the object passed
    cacheMatrix$setInverse(inverse)
    # return inverse
    inverse
}
