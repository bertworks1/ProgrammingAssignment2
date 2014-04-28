## ---------------------------------------------------------------

## These functions provide a means for caching the inverse of a
## matrix so that it does not have to be recomputed. If a cached
## value does not exist, the value is computed and cached.

## ---------------------------------------------------------------

## Create a "matrix" object capable of caching its inverse. The 
## object has methods set(), get(), setinverse(), and getinverse(). 

makeCacheMatrix <- function(x = matrix()) 
{
    xInverse <- NULL   # initial value
    
    # Define the methods and then return them as a list. 
    
    set <- function(y) 
    {
        x <<- y
        xInverse <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) xInverse <<- inverse
    getinverse <- function() xInverse
    
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of matrix 'x' (assumed to be invertible).
## A cached value is returned if available; otherwise the 
## inverse is computed and cached.

cacheSolve <- function(x, ...) 
{
    # First, check to see if there is a cached value of the
    # matrix inverse. If so, return it.
    
    xInverse <- x$getinverse()
    if(!is.null(xInverse)) 
    {
        message("getting cached inverse")
        return(xInverse)
    }
    
    # There was no cached value, so compute the inverse and
    # then cache it and return it.
    # (Normally there should be checks to be sure the matrix
    # is square, invertible, etc., but for this assignment,
    # the matrix is assumed to be invertible.)
    
    xInverse <- solve(x$get())
    x$setinverse(xInverse)
    xInverse
}
