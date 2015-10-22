#### Programming Assignment 2 solution

### This script contains two functions:

## makeCacheMatrix() 
# creates a special "matrix" object that can cache its inverse.
# it returns a list of four functions (see inside)

## cacheSolve() 
# computes the inverse of the special "matrix" returned by makeCacheMatrix 
# returns and caches the inverse

## Detailed explanations are provided within the function

## Below the functions I also added an example that shows how the functions work

makeCacheMatrix <- function(mat = matrix()) {
	## Function creates a special "matrix" object that can cache its inverse.
	## returns a list of four functions: 
	# set() to set the value of the matrix
	# get() to get the value of the matrix
	# setinv() to cache the value of the inverse matrix
	# getinv() to get the value of the inverse matrix
 	mat.inv <- NULL
 	set <- function(mat.set) {
   		mat <<- mat.set
		mat.inv <<- NULL
 	}
	get <- function() mat
	setinv <- function(inv) mat.inv <<- inv
	getinv <- function() mat.inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(mat, ...) {
	# computes the inverse of the special "matrix" returned by makeCacheMatrix 
	# if inverse exists already, cacheSolve returns it from cache
	mat.inv <- mat$getinv()
	if(!is.null(mat.inv)) {
		message("getting cached data")
		return(mat.inv)
	}
	data <- mat$get()
	mat.inv <- solve(data, ...)
	mat$setinv(mat.inv)
	mat.inv
}


# ## Example usage:
# # Create a large, square example matrix
# mat.new <- matrix(sample(1:10^4, size = 1024), ncol = 32)
# # Feed this into makeCacheMatrix() and store myMat
# myMat <- makeCacheMatrix(mat.new)
# myMat$get() # returns the matrix
# cacheSolve(myMat) # returns and caches the inverse
# myMat$getinv() # returns the cached inverse
