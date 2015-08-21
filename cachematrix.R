## Implementation of functions exibiting caching of difficult to
## calculate values. This is done by using to <<- operator to save
## values calculated in nested functions to the parent function's
## scope, allowing other nested values to work with these values.


## This function takes a matrix as an argument. I returns an "object"
## that provides a series of functions for retrieving and setting the
## matrix and it's inverse. It does not however calculate its own
## inverse. The set() function is also responsible for setting the
## inverse of the matrix to null. This is done so that other functions
## working with these "objects" whether to set the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function(){
        x
    }
    getInverse <- function(){
        inverse
    }
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    setInverse <- function(m) inverse <<- m
    list(get = get, getInverse = getInverse, set = set, setInverse = setInverse)
}


## This function receives a matrix "object" and is responsible
## computing the inverse of the "objects's" matrix. It ONLY does this
## however, if the inverse of the "object's" matrix is null.
cacheSolve <- function(x) {
    i <- x$getInverse()
    if(is.null(i)) {
        i <<- solve(x$get())
    }
    i
}
