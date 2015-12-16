## Two functions that aim at optimizing the process of computing the inverse of a matrix (assuming the matrix is invertible)
## example:
## B <- matrix( c(2, 4, 3, 1, 5, 7, 7, 8, 9), nrow=3, ncol=3) ## this creates a new invertible matrix
## cB <- makeCacheMatrix(B) ## cacheable version of B
## iB <- cacheSolve(cB) ## computes inverse of B, since this is the first call, it will perform normal computation
## ib2 <- cacheSolve(cB) ## computes inverse of B but since this is the second call, it will get the result from cache without recomputing

## makeCacheMatrix "upgrades" any matrix to a cacheable matrix. Via the <<- operator, the result is assigned to an object in an environment that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
	## 'matrix' is the input matrix to be inverted - we assume here that the matrix is invertible
	## Return the cacheable matrix that can be accessed from another environment

	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, 
	setinverse = setinverse,
    getinverse = getinverse)
}

## cacheSolve does the actual computation of the inverse matrix by taking as input the "cacheable matrix", produced by the previous function

cacheSolve <- function(x, ...) {
	## 'x' is a cacheable matrix to be inverted as produced by the makeCacheMatrix function
	## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
	if(!is.null(i)) { ## if the inverted matrix is already computed
		message("getting cached data")
		return(i)  
	} ## if the inverted matrix is not computed, then use the solve function and store the result
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
