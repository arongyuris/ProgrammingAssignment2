## This function serves to store an invertible matrix(x) in a cache for retreival by the function cacheSolve 
#  
# makeCacheMatrix <- function(x = matrix()) {           ## Defines function makeCacheMatrix and sets arguments as variable x which is an invertible matrix(x)
#       m <- NULL                                           ## Sets the variable m to be clear for storage of an input matrix(x)
#       set <- function(y) {				                ## Defines variable set as a function to:
#                x <<- y                                	    ## Set the value of matrix(x) to variable y in the parent environment (i.e. function makeCacheMatrix)
#                m <<- NULL                             	    ## Resets value of variable m as empty in the parent environment
#        }
#       get <- function() x                             ## Defines variable get as a function to retrieve the value of matrix(x) when the function get is called
#       setsolve <- function(solve) m <<- solve         ## Defines variable setsolve as a function to use the solve function to set the variable m to the inverse of matrix(x)
#       getsolve <- function() m                        ## Defines variable getsolve as a function to retreive cached variable m, which has been set as the inverse of matrix(x)
#               list(set = set, get = get,                      ## Defines output "special matrix" as a list of the related functions above to be used as input for the function cacheSolve
#			        setsolve = setsolve,					
#                   getsolve = getsolve)                    
# }
#
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function serves to retrieve a cached invertible matrix(x) from function makeCacheMatrix and in its absence evaluate the inverse 
## of a matrix and store it in the cache 
#
# cacheSolve <- function(x, ...) {              ## Defines function cacheSolve and sets arguments as "special matrix" output from the function makeCacheMatrix
#       m <- x$getsolve()                       		## Defines variable m as cached value of function getsolve from function makeCacheMatrix
#       if(!is.null(m)) {                       		## Uses If conditional to return the value of m if m is NOT null  
#       message("getting cached data")          		## Prints a message stating that cached value of m is being retrieved
#       return(m)                               		## Prints the cached value of m
# }
#       data <- x$get()                         		## If no value of m has been cached, stores value of matrix(x) from function makeCacheMatrix as the variable data
#       m <- solve(data, ...)                   		## Stores value of inverse of matrix (x) as variable m
#       x$setsolve(m)                           		## Caches variable m using the setsolve function from the function makeCacheMatrix
#       m                                       		## Prints the cached value of m
# }
#
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
