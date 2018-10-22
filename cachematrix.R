## this is Programming assigment 2, that caches invers of the matrix
## 

## following function caches the matrix and inverse of the matrix and creates list of 4 functions

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	invrs <- NULL
	set <- function(y) {
			x <<- y
			invrs <<- NULL
			}   ## set is a function that changes the matrix stored in the main function sets invers to NULL
	get <- function() x
	setinverse <- function(inverse) invrs <<- inverse
	getinverse <- function() invrs
	list(set = set, get = get, setinverse = setinverse,
	      getinverse = getinverse)  ## store the 4 functions 
                                   ## so that when we assign makeCacheMartix to an object, the object has all the 4 functions
 }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invrs <- x$getinverse()  ## getinvers from cache
	if(!is.null(invrs)) {
		message("getting cached data")
		return(invrs)
  		}  ## verify the value invr, stored previously exists and is not NULL
   
	mat <- x$get()  ## this gets matrix from cache 
	invrs <- solve(mat, ...)  ## solve function calulated inverse of matrix x
	x$setinverse(invrs)
	invrs  ## returns inverse of x

 }
