## The below functions solve the inverse of a matrix. If possible, the inverse is pulled from cached memory

## Gets and sets the cache of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL
	
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	
}


## Attempts to get the inverse of the matrix through the cache or creates it and caches it
cacheSolve <- function(x, ...) {

	inverse <- x$getinverse()
	
	if(!is.null(inverse)) {
		message("Cache Data Being Retrieved...")
		return(inverse)
	}
	
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	
	inverse
}