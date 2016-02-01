## Description: Caching the inverse of a matrix rather than compute it repeatedly
## Usage: m <- makeCacheMatrix(mat)
##        cacheSolve(m)


## function "makeCacheMatrix" creates a square "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	## initialize the value of the matrix inverse to NULL
	mat_inv <- NULL
	
	## set the value of the matrix and matrix inverse
	set <- function(y) {
		x <<- y
		mat_inv <<- NULL
	}
	
	## gets the value of the matrix
	get <- function() x
	
	## sets the inverse of the matrix
	setinverse <- function(solve) mat_inv <<- solve
	
	## gets the inverse of the matrix
	getinverse <- function() mat_inv
	
	## passes the value of the function makeCacheMatrix
	list(set = set, get = get, 
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## function "cacheSolve" calculates the inverse of the matrix created with the "makeCacheMatrix" function.
## It gets the inverse from the cache and skips the computation if the matrix has already been calculated.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	mat_inv <- x$getinverse()
	
	## if the inverse exists, gets from cache.
	if(!is.null(mat_inv)) {
		message("getting cached data")
		return(mat_inv)
	}
	
	## if inverse doesn't exist, then it is calculated and returned.
	data <- x$get()
	mat_inv <- solve(data, ...)
	x$setinverse(mat_inv)
	mat_inv
}
