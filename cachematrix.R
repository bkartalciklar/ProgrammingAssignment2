################################
## makeCacheMatrix: creates a special "matrix", which is a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
    minv <- NULL
    set <- function(y){
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(binv) minv <<- binv
    getinv <- function() minv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

################################

# cacheSolve: calculates the inverse of the special "matrix" created with the "makeCacheMatrix" function. It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...){
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("getting cashed data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
}
