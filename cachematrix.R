## Put comments here that give an overall description of what your
## functions do

## This functions creates a special "matrix", which is really a list 
## containing a function to 
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix inverse
## 4) get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) 
{
    # when creating or changing the matrix values, initialize inverse to NULL
    inverse <- NULL
    
    # 1) set the value of the matrix
    set <- function(y) 
    {
        x <<- y
        inverse <<- NULL
    }
    
    # 2) get the value of the matrix
    get <- function() x
    
    # 3) set the value of the matrix inverse
    setInverse <- function(inv) inverse <<- inv
    
    # 4) get the value of the matrix inverse
    getInverse <- function() inverse
    
    # returned object is a list of functions to interact with the data x
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the matrix invertion of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) 
{
    # Get the cached inverse value from the object
    inverse <- x$getInverse()
    # if the cached value exists, return that value
    if(!is.null(inverse)) 
    {
        message("getting cached data")
        return(inverse)
    }
    
    # if there is no cache value, calculate the inverse
    data <- x$get()
    inverse <- solve(data, ...)
    # and chache it!
    x$setInverse(inverse)
    
    inverse
}
