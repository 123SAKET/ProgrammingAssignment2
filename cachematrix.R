## Function to cache the inverse of a special 'matrix' object 
## to avoid repeating potentially time-consuming computations
makeCacheMatrix <- function(x = matrix()) {
## creates a special 'matrix' object that can cache its own inverse
        I <- NULL
        set <- function(y) {
                 x <<- y
                 I <<- NULL
        } 
        get <- function() x
        setI <- function(solve) I <<- solve
        getI <- function() I
        list(set = set, get = get, setI = setI, getI = getI)
        ## returns a list of the functions for setting and getting the 
        ## 'matrix' object and its inverse
}
cacheSolve <- function(x, ...) {
## takes the instance of the makeCacheMatrix function, if inverse
## has already been computed, retrieves inverse from cache
         I <- x$getI()
         if(!is.null(I)) {
	                 message ("getting cached data")
                 return(I)
         }
        ## computes the inverse of the special 'matrix' returned by 			
        ## makeCacheMatrix if inverse not found in cache
         data <- x$get()
         I <- solve(data, ...)
         x$setI(I)
         I
 }
 
