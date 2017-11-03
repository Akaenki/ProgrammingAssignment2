## This funcion create a list of functions

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y){
        x <<- y
        xinv <<- NULL
    }
    get <- function() x 
    setxinv <- function(y) xinv <<- y
    getxinv <- function() xinv
    
    list(set = set, get = get, setxinv = setxinv, getxinv = getxinv)
}


## This function check if the inverse matrix is in the cache, if not will calculate
## the inverse and cache it 

cacheSolve <- function(x, ...) {
    xinv <- x$getxinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    message("calculate and cache data")
    xinv <- ginv(data)
    x$setxinv(xinv)
    xinv
}
