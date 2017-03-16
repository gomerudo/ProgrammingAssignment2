## This R script declares 2 functions:
##      1. makeCacheMatrix: Encapsulates a matrix and its inverse into an object
##      2. cacheSolve: Resolves the inverse of the object returned by (1)
## The purpose is to reduce redundant computing that spends time. We compute the
## inversed matrix for each set of data only. 

## This function encapsulates a matrix and its inverse. It provides the next
## methods:
mcm <- function(x = matrix()) {
    # Initialization
    matrixInverse <- NULL
    
    # Nested setter for  data (matrix)
    setMatrix <- function(y){
        # Point to new data (passed as argument). Upper environment set
        x <<- y
        # Re-init of inversed matrix. Old one is deprecated because new input
        # Upper environment set
        matrixInverse <<- NULL 
    }
    
    # Nested getter for data (matrix)
    getMatrix <- function() x # Returns data
    
    # Nested setter for Matrix Inverse
    setMatrixInverse <- function(matrixInverseArg){ 
        # Upper environment set
        matrixInverse <<- matrixInverseArg
    }
    
    # Nested getter for Matrix Inverse
    getMatrixInverse <- function() matrixInverse
    
    # Return a list of all functions declared above
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## This functions returns the cached matrix inverse of the object returned by
## 'makeCacheMatrix', if available, otherwise it calculates it, store it and 
## returns the computed inverse.
cacheSolve <- function(x, ...) {
    # Get inverse in object
    matrixInverse <- x$getMatrixInverse()
    
    # If the matrix inverse is already computed, return it
    if(!is.null(matrixInverse)){
        message("Inversed matrix in cache, ")
        return(matrixInverse)
    }
    
    # Otherwise, we get the original matrix and compute inversed for the first 
    # time
    matrix <- x$getMatrix()
    matrixInverse <- solve(matrix)
    
    # Set the mean indicating that the value has already been computed
    x$setMatrixInverse(matrixInverse)
    
    # Return matrix inverse
    matrixInverse
}
