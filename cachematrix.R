## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ci <- NULL
    set <- function(y) {
        x <<- y
        ci <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ci <<- inverse
    getinverse <- function() ci
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse );
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
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
    x$store(cm)             ## Store the cached mean
    cm                      ## And return it.
}
