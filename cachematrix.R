## Below are two functions that work together to use a cached value of the inverse of a 
## matrix if it is available

## makeCacheMatrix creates a list of functions that set or get the value of the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()){
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## cacheSolve returns the inverse of a matrix - first by checking if there is a value in the cache failing which 
##it calculates and returns the inverse

cacheSolve <- function(x,...){
        ## Return a matrix that is the inverse of 'x'
	inv <-x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}
