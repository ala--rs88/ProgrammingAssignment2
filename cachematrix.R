
## This function creates special "matrix" object which is responsible for storing the value of the matrix 
## and for caching the value of it's inverse matrix. Optional argument 'x' specifies the initial value of matrix. 
## Cached value of inverse matrix is assigned by calling setCachedInverseMatrix(inverseMatrix) and is set to NULL 
## every time set(newMatrix) gets called.
makeCacheMatrix <- function(x = matrix()) {
        cachedInverseMatrix  <- NULL
        set <- function(newMatrix) {
                x <<- newMatrix
                cachedInverseMatrix <<- NULL
        }
        get <- function() {
                x
        }
        setCachedInverseMatrix <- function (inverseMatrix) {
                cachedInverseMatrix <<- inverseMatrix
        }
        getCachedInverseMatrix <- function () {
                cachedInverseMatrix
        }
        
        list(set = set,
             get = get,
             setCachedInverseMatrix = setCachedInverseMatrix,
             getCachedInverseMatrix = getCachedInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function using solve function.
## Argument 'x' corresponds to "matrix", argument '...' is passed to solve function along with value of matrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getCachedInverseMatrix()
        
        if(is.null(inverseMatrix)) {
                matrix <- x$get()
                inverseMatrix <- solve(matrix, ...)
                x$setCachedInverseMatrix(inverseMatrix)
        } else {
                message("getting cached data")
        }
        
        inverseMatrix
}
