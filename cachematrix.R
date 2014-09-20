## Put comments here that give an overall description of what your
## functions do

## Function that returns a list of functions operating on locally scoped objects

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) i <<- inverse
	getinv <- function i
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of a matrix by computing or hitting the local cache inside the above function objects

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if(!is.null(i)){
		message("Local Cache Hit")
		return(i)
	}
	mat <- x$get()
	inverse <- solve(mat, ...)
	x$setinv(inverse)
	inverse
}
