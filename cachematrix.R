## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {   ##'x' defined with argument requiring square, inversible matrix
    s <- NULL                                 ## 's' set to NULL in immediate environment
    set <- function(y) {                      ## nested 'set' function with 'y' as argument 
        x <<- y                               ## 'x' set to 'y' in global environment
        s <<- NULL                            ## 's' set to NULL in global environment
    }
    get <- function() x                       ## creates 'get' function using 'x' from user-defined argument
    setsolve <- function(solve) s <<- solve   ## Creates the setsolve sub-function 
    getsolve <- function() s                  ## creates the getsolve sub-function
                                              ## this generates the list input to cacheSolve()
    list (set = set,
          get = get,
          setsolve = setsolve,
          getsolve = getsolve)      
}


## This function checks the cache for the inverse and, if not found computes the inverse

cachesolve <- function(x=matrix(), ...) {     ##takes the list from makeCacheMatrix function and returns inverse from cache or calculation
        s <- x$getsolve()                     ## populates 's' with 'x' (inverted matrix) from makeCacheMatrix function output
        if(!is.null(s)) {                     ## with 's' retrieved from cache, checks if its not NULL, 
        message("getting cached data")
        return(s)                             ## if so, returns 's' from cache
    }
    data <- x$get()                           ## if else, gets the original matrix, and puts it in 'data'
    s <- solve(data, ...)                     ## creates inverse and applies 'solve' function to 'data'             
                                              ##    and puts it in 's' in immediate environment
    x$setsolve(s)                             
    s                                         
}                             
