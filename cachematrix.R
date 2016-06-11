## Cashing the Inverse of a Square Matrix
## 1.) makeCacheMatrix creates a special matrix obj
## 2.) cacheSolve calculates the inverse of the matrix
## 3.) test if matrix inverse has already been calculated
##      a.) if yes it will find cache and return it
##      b.) if not it will calculate it again


makeCacheMatrix <- function(x = matrix()) {
	invrs <- NULL
	set <- function(y){
		x <<- y
		invrs <<- NULL
}	
	get <- function()x
	setinverse <- function(inverse)invrs <<- inverse
	getinverse <- function() invrs
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve does steps 2 and 3 listed above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invrs <- x$getinverse()
	if(!is.null(invrs)) {
	message("getting cached data")
	return(invrs)
}
	mat <- x$get()
	invrs <- solve(mat, ...)
	x$setinverse(invrs)
	invrs
}
