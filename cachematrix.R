## The Main idea is to use "<<-" operator to store the value which can be used later but required a lot
## of time to calculate. The first function makeCacheMatrix creates a object store a input matrix and a
## inverse matrix of the input. The second function cacheSolve solves the inverse matrix and stored into
## the object.
## 
## Example :
## matrixData <- rbind(
##     c(1, -4, 3), 
##     c(7, 1, 5), 
##     c(2, -1, 1) 
## )
## cacheData <- makeCacheMatrix(matrixData)
## cacheSolve(cacheData)
##             [,1]        [,2]       [,3]
## [1,] -0.18181818 -0.03030303  0.6969697
## [2,] -0.09090909  0.15151515 -0.4848485
## [3,]  0.27272727  0.21212121 -0.8787879

## This object has 1 property "inverse" to store the inverse matrix of the original matrix
## and 4 methods get(), set(), getInverse(), setInverse()
## get() actually works like a property, it return the input matrix
## set() allows user to change the matrix value after the cacheMatrix is created.
## getInverse() returns the inverse matrix of the input matrix
## setInverse() allows user to change the inverse matrix. It is mainly used by the cacheSolve function
## to save the solved inverse matrix into the cacheMatrix object.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(mtx){
		x <<- mtx
		inverse <<- NULL
	}
	get <- function() x 
	setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
	getInverse <- function() inverse
	list(
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}

## The cacheSolve take the cacheMatrix object as input and return the inverse matrix.
## If there is already a inverse matrix in cacheMatrix, cacheSolve simply return the exiting value.
## If not, cacheSolve calculate the inverse matrix of the cacheMatrix object.
## Besides, not every matrix is invertible. If the input matrix is not invertible, an error message
## will be printed and return the inverse matrix as NULL.

cacheSolve <- function(x, ...) {
	# To get the inverse matrix, we first need to get the original input matrix
    inverse <- x$getInverse()
    # Then we check if the inverse already exists. If so, return it and break the function.
    if(!is.null(inverse)){
    	message("Inverse Matrix already exists. Returning cached data >")
    	return(inverse)
    }
    # If there is no previous inverse value(NULL), we calculate the inverse matrix from the original input matrix.
    originalMatrix <- x$get()
    # Use try() to catch the error message when the input matrix is not invertible and keep the excution continuing.
    try(inverse <- solve(originalMatrix), TRUE)
    if(is.null(inverse)){
    	#replace the error message of the solve function with more understandable message.
    	message("ERROR: The matrix is exactly singular matrix which is not invertible.")
    }
    x$setInverse(inverse)
    inverse
}
