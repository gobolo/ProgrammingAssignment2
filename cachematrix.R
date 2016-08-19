##  this function caches the inverse of a matrix
## it allows the user to not waste time doing it repeatedly 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x<<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(mean) m <<- mean
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## this function allows the user to user to compute the inverse of the matrix returned by the makeCache Matrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
        		message("getting cached data")
        		return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$getInverse(m)
        m
}
