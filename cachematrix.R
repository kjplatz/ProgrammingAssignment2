## Function makeCacheMatrix( x=matrix() )
##
## Create an object that contains a matrix and its (possibly cached)
## inverse.
##
## Defined fields:
##     get() - returns the original matrix
##     getinverse() - returns the (cached) inverse
##     setinverse() - stores the inverse (in cache)

makeCacheMatrix <- function(x = matrix()) {
    ci <- NULL
    set <- function(y) {
        x <<- y
        ci <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ci <<- inverse
    getinverse <- function() ci
    list( set = set, get = get, 
          setinverse = setinverse, getinverse = getinverse );
}

## Function cacheSolve(x, ...)
##
## Return a matrix that is the inverse of 'x'
## This will attempt to access the cached inverse 
## (via getinverse()) if available.
cacheSolve <- function(x, ...) {
  
    ## Attempt to retrieve the cached inverse matrix
    cm <- x$getinverse()

    ## if the result is non-NULL, return it
    if ( !is.null(cm) ) {
        message("Getting cached data")
        return(cm)
    }

    ## If we've made it this far, the value has NOT been cached.
    data <- x$get()         ## Get the stored data
    cm <- solve(data,...)   ## Calculate the inverse
    x$setinverse(cm)        ## Store the cached inverse
    cm                      ## And return it.
}
