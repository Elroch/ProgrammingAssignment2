## These two functions serve to provide a "matrix" whose inverse may be needed 
## on more than one occasion.
## The first time the inverse is needed it is calculated, but on later
## occasions, the cached inverse is retrieved.

## The first function creates a special "matrix" which is really a list
## containing four functions which:
## 1. set the value of the matrix
## 2. get the value of the matrix 
## 3. set the value of the inverse of the matrix 
## 4. set the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    ## first initialise the inverse to null to indicate it needs to be 
    ## calculated
    inv <- NULL
    
    ## when the matrix is created (or changed), the cached inverse needs to be 
    ## flagged as uncalculated by setting it to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## the get function simply retrieves the current matrix
    get <- function() {
        x
    }
    
    ## the setinverse function sets the inverse matrix to a specified matrix
    setinverse <- function(inverse) {
        inv <- c(dim(inverse)[1], dim(inverse)[2], as.numeric(inverse))
        inv <- inverse
    }
       
    ## the getinverse function returns NULL if it hasn't been calculated yet
    ## or the correct inverse of x if it has
    getinverse <- function()  {
        return(inv)
    }
    
    ## returns a list of all four functions
    list(set = set,
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## The following functon calculates the inverse of the special "matrix" created 
## with makeCacheMatrix. First it checks if the inverse has already been
## calculated. If so it returns the inverse from the cache without further
## calculation. If not it calculates the inverse and sets the value of the
## inverse using the setInverse function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
