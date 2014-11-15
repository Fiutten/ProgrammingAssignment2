## Two functions for caching special matrix and solving the inverse of it.

## Caches the special matrix with its getters and setters. 
## Encapsulates the process making it transparent to the user.

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL

	## Method to store the original matrix.
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	## Method to obtain the original matrix.
	get <- function() {
		x
	}

	## Method to store the inverse matrix.
	setinverse <- function(inverse) {
		i <<- inverse
	}

	## Method to obtain the inverse matrix.
	getinverse <- function() {
		i
	}
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of a cached matrix.
## If the inverse has already done it will be returned without any operation.
## Optionally it checks if the matrix is square.

cacheSolve <- function(x, ...) {

	## Returns a matrix that is the inverse of 'x'.
	i <- x$getinverse()

	## Optionally checks if the matrix is square.
	if (nrow(x$get()) != ncol(x$get())) {
		message("matrix must be square")
		return (x)
	}
	
	## Checks if the inverse has already calculated.
	if (!is.null(i)) {
		message("matrix already inverted")
		return(i)
	}
	
	## Executes the action, storing the inverse and returning as a result.
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
