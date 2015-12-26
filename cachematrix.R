## Put comments here that give an overall description of what your
## functions do
##
##   A set of functions designed to minimize computation of matrix inverses
##   by saving the inverse as an attribute in a special matrix object.
##
##   makeCacheMatrix
##       a constructor for a matrix like object that caches its inverse
##
##   cacheSolve
##       generates matrix invese taking advantage of the attributes
##       of an object constructed with makeCacheMatrix.
##       If the inverse is cached, this cached value is returned.
##       Otherwise, the inverse is computed using solve function, stored in cache,
##       and its value returned.

##   Function
##   makeCacheMatrix
##      args: x = matrix()
##      methods:
##         set: args: matrix
##         get: return the matrix
##         setinv: sets the inverse in cache
##         getinv: returns the inverse
##      returns a list object that encapsulatese the methods for access using the
##      object$method syntax. See examples below. 
##
##      This is a constructor for a matrix object that holds its inverse
##      as an attribute, i.e. cached, once it is computed. Access to this
##      attribute, i.e. matrix inverse, is through the methods "setinv" and "getinv".
##      Methods "get" and "set" allow access and reset to the values of the matrix.
##
##      Matrices built with this constructor are expected to call the cacheSolve
##      function in this package to compute the inverse, and benefit from the cache
##      inverse to avoid unnecessary computations.
##      See details below for the cacheSolve function.
##
##      Usage examples:
##> source("cachematrix.R")
##> set.seed(1)
##> mcache <- makeCacheMatrix(matrix(rnorm(9),nrow=3,ncol=3))
##> mcache$get() 
##           [,1]       [,2]      [,3]
##[1,] -0.6264538  1.5952808 0.4874291
##[2,]  0.1836433  0.3295078 0.7383247
##[3,] -0.8356286 -0.8204684 0.5757814
## 
##> mcache$getinv()
##NULL
##
##> mcache$setinv(cacheSolve(mcache))
##> mcache$getinv()
##            [,1]        [,2]       [,3]
##[1,] -0.50015875  0.82896132 -0.6395669
##[2,]  0.45439113 -0.02930499 -0.3470881
##[3,] -0.07838637  1.16130885  0.3139817
##
##> cacheSolve(mcache)
##getting cached data
##            [,1]        [,2]       [,3]
##[1,] -0.50015875  0.82896132 -0.6395669
##[2,]  0.45439113 -0.02930499 -0.3470881
##[3,] -0.07838637  1.16130885  0.3139817
##
##> mcache$get()%*%mcache$getinv()
##             [,1]         [,2]          [,3]
##[1,] 1.000000e+00 1.110223e-16 -1.942890e-16
##[2,] 0.000000e+00 1.000000e+00  2.775558e-17
##[3,] 2.081668e-17 0.000000e+00  1.000000e+00
##
##> mcache$set(matrix(rnorm(9),nrow=3,ncol=3))
##> mcache$get()
##           [,1]       [,2]        [,3]
##[1,] -0.3053884 -0.6212406 -0.04493361
##[2,]  1.5117812 -2.2146999 -0.01619026
##[3,]  0.3898432  1.1249309  0.94383621
##> mcache$getinv()
##NULL

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(y) { inv <<- y  }
    getinv <- function() inv 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##   Function
##   cacheSolve
##      args:
##           x: a CacheMatrix object
##           ... passed on to built-in R function "solve"
##      returns
##           cached inverse of x
##           or computed inverse using built-in function "solve"
##               if computed, it is also cached
##
## Usage examples:
##
##> source("cachematrix.R")
##> set.seed(1)
##> mcache <- makeCacheMatrix(matrix(rnorm(9),nrow=3,ncol=3))
##
##> cacheSolve(mcache)
##            [,1]        [,2]       [,3]
##[1,] -0.50015875  0.82896132 -0.6395669
##[2,]  0.45439113 -0.02930499 -0.3470881
##[3,] -0.07838637  1.16130885  0.3139817
##
## THE CALL TO cacheSolve(mcache) NOT ONLY COMPUTES THE INVERSE, AND RETURNS IT, BUT
## ALSO SETS IT IN CACHE FOR FUTURE ACCESS THROUGH THE "getinv" METHOD.
##
##> mcache$getinv()
##            [,1]        [,2]       [,3]
##[1,] -0.50015875  0.82896132 -0.6395669
##[2,]  0.45439113 -0.02930499 -0.3470881
##[3,] -0.07838637  1.16130885  0.3139817
##
##    CAVEAT!
##    THIS FUNCTION DOES NOT WORK WITH REGULAR MATRICES
##    IT NEEDS THE CACHE VERSION CONSTRUCTED WITH makeCacheMatrix
##    mcache$get()  return a native matrix object that lacks the "get"
##    attribute.
##
##> cacheSolve(mcache$get())
##Error in x$getinv : $ operator is invalid for atomic vectors
##

cacheSolve <- function(x, ...) {
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
