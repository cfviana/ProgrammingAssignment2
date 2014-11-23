# makeCacheMatrix creates a special "matrix" object 
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

# set inverse matrix to NULL, so that if the matrix is 
# re-assigned it invalidates the previously computed inverse.
	m<-NULL  

# set the value of the matrix	
	setMatrix<-function(y){
		x<<-y
		m<<-NULL
	}

# get the value of the matrix	
	getMatrix<-function() x

# Computing the inverse of a square matrix	
	cacheInverse<-function(solve) m<<- solve
	
	getInverse<-function() m
	
	list(setMatrix=setMatrix, getMatrix=getMatrix,
	   cacheInverse=cacheInverse,
	   getInverse=getInverse)
}

# cacheSolve is a function that computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
	m<-x$getInverse()
	
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

# get the value of the matrix	
	matrix<-x$getMatrix()

# get the inverse of a square matrix	
	m<-solve(matrix, ...)
	x$cacheInverse(m)
	m
}