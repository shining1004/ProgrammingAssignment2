## There are two functions in the R file: makeCacheMatrix and cacheSolve. 
## They can be used to save computing time for Matrix inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mi<- NULL
    set <- function(m){
        x<<-m
        mi<<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) mi<<-solve
    
    getsolve<- function() mi
    
    list(set = set, get = get, setsolve=setsolve, getsolve=getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mi <- x$getsolve()
    if (!is.null(mi)){
        message("getting cached data")
        return (mi)
    }
    mdata<-x$get()
    mi<- solve(mdata)
    x$setsolve(mi)
}
