## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
###....basically exchange mean with solve from the example


makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
        
        ## Write a short comment describing this function
        
        # The following function calculates the mean of the special "vector"
        # created with the above function. However, it first checks to see if 
        # the mean has already been calculated. If so, it gets the mean from the cache and skips the computation.
        # Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
        
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting inversed matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
        
        
        