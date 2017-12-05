## These functions, in conjunction, produce a list of operations to
## perform on a given matrix input, and also contain data in a persistent
## function environment that allows for the storage of a cached matrix
## inverse.  When the cacheSolve function is called, it checks the value
## of the variable holding the matrix inverse values in the makeCacheMatrix
## environment.  If the the variable contains NULL, the inverse is computed
## from scratch.  Otherwise, the function skips the inverse calculation and
## returns the matrix inverse that has already been solved for.



## makeCacheMatrix produces a function list defining data setting
## and retrieval operations, and produces a persistent function
## environment that holds information on whether or not the matrix
## inverse has already been computed for a given input

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    l <- list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
    return(l)
}


## cacheSolve checks the value of "i", defined in the makeCacheMatrix 
## function enviornment.  If i = NULL, (indicating the inverse had not 
## previously been calculated), the matrix inverse is solved for.
## otherwise, the cached value of the matrix inverse is returned.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    message("computing matrix inverse")
    return(i)
}
