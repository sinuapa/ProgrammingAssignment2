## The functions makeCacheMatrix and cacheSolve work together to minimize
## processing of the inverse of a matrix.  cacheSolve checks to see in the 
## inverse of a matrix exists in cache; if it does, it simply returns it. 
## If not, it computes and returns that.

##  This function solves for the inverse of a matrix and leaves it in cache
##  via the <<- operator.

makeCacheMatrix <- function(x = matrix()) {
  	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function will use a previously solved inverse of a matrix.
## Otherwise, it will produce the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
