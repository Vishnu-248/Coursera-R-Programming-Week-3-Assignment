## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## defines argument with default "matrix" mode
		inv <- NULL				  ## initialize inv as 'NULL'.It will hold value of matrix inverse
        	set <- function(y) {		  ## define the s function to assign new 
            		x <<- y		  ## value of matrix in parent environment
                		inv <<- NULL	  ## if there is a new matrix, reset inv to NULL
        	}
        	get <- function() x			  ## define the g fucntion - returns value of the matrix argument
        	setInv <- function(inverse) inv <<- inverse		## assigns value of inv in parent environment
        	getInv <- function() inv					## gets the value of inv where its called
        	list(set = set,							## to refer functions with the '$' operator
            get = get,
            setInv = setInv,
            getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getInv()
		if(!is.null(inv)) {
			message("Cached data")
        		return(inv)
   		}
    		data <- x$get()
   		inv <- solve(data, ...)
   		x$setInv(inv)
   		inv

}
