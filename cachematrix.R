## These functions are intended to cache the inverse of a matrix
## for faster computations

## makeCacheMatrix takes a matrix and creates a list 
## of parameters so that the inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(solve) i <<- solve
	getinv <- function() i
	list( set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## cacheSolve computes the inverse of a matrix.
## If the inverse has previously been computed,
## cacheSolve returns the cached matrix. 

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}

