## Return the Inverse of a Matrix (Considering the fact that the matrix is square).
## If the inverse has been calculated already, it returns the inverse from cache.

## makeCachedMatrix creates a list special matrix and return foru functions:
##  1. Set the Matrix
##  2. Get the Matrix
##  3. Set the inverse of the Matrix
##  4. Get the inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)      
}


## Returns a matrix that is the inverse of x
## If the inverse already exsit,bring the reuslts from the cached data
##If the matrix does not exits, then gets and returns the inverse
cacheSolve <- function(x, ...) {
        inv = x$getinv()     
        # if the inverse has already been calculated, gets it from cahce and skips execution.
        # otherwise, calculates the inverse using the solve function.
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matx.data = x$get()
        inv = solve(matx.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        # returns the inverse of the matrix.
        return(inv)
}
