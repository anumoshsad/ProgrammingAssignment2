## Below are two functions that are used to create a special 
## object that stores a numeric matrix and caches its inverse.

## This function creates a special "matrix" object that can cache
## its inverse. This is actually a list of 4 functions to set the 
## matrix, get the matrix, set the inverse, get the inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) m <<- Inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache via the getInv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m        
}
