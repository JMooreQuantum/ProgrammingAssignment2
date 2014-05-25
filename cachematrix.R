## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        
        ## start by setting the m variable too a null 
        ## then create four functions and put them in a list when done
        ## the four functions are:
        ## set ... sets the global matrix object 'x' equal to input matrix 'y'
        ##         and also resets the m to a null
        ## get ... gets the global matrix object 'x'
        ## setsolve ... sets the global matrix object 'x' equal to 
        ##         the inverted matrix
        ## getinv ... gets the cached inverted matrix if it already exists
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getinv = getinv)
        
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## first use the getinv function to see if an inverse already exists
        ## if it does then just use the cached data and use 'return' to 
        ## break out of the routine
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If there is no cached inverse, then use the get function
        ## to retrieve the matrix, then use the solve function to invert
        ## the matrix, then the setsolve function to set it to the 
        ## global namespace variable 'm' 
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        ## pass the inverted matrix back out 
        m        
                
}
