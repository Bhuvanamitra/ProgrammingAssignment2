## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m = matrix() 	#create am empty matrix to store the inverse
	m <- NULL
    	
	set <- function(y = matrix()){
        	x <<- y		
		m <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(solve) m <<- solve	#this will keep inverse in cache
	
	getInverse <- function() m			#if inverse is in cache, return it
	
	list(set = set, get = get, 
		setInverse = setInverse,
		getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()				#try to get inverse from cache
	if(!is.null(m)){
      		message("getting cached data")
          	return(m)				#return the value if present in cache
	}
	
	data <- x$get()					#if not present, get the matrix
	
	m <- solve(data, ...)				#find the inverse of it
	
	x$setInverse(m)					#push it to cache
	
	m						#print to console
}
