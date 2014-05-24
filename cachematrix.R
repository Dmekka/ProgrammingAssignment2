## R function to cache potentially time-consuming matrix computations.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	invs <- NULL
	
	set <- function(y) {
			x <<- y
			invs <<- NULL
	       }
	
	get <- function() x
	
	setinverse <- function(inverse) invs <<- inverse
	
	getinverse <- function() invs
	
	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        
	if( !is.null(invs) ) {
		return(invs)
	}
	
	data <- x$get()
	
	invs <- solve(data, ...)
	
	x$setinverse(invs)
	
	invs
}
