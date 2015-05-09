## Put comments here that give an overall description of what your
## functions do
##
##   makeCacheMatrix
##       a constructor for a matrix like object that caches its inverse
##
##   cacheSolve
##       generates matrix invese takign advantage of cache
##       or using R method solve to compute the inverse

## Write a short comment describing this function
##   makeCacheMatrix
##      args: x = matrix()
##      methods:
##         set: args: matrix
##         get: return the matrix
##         setinv: sets the inverse in cache
##         getinv: returns the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(y) { inv <<- y }
    getinv <- function() inv 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
##   cacheSolve
##      args:
##           x: a CacheMatrix object
##           ... passed on to solve
##      returns
##           cached inverse of x
##           or computed inverse using built-in function "solve"
##               if computed, it is also cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv 
}
