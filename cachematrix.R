## Put comments here that give an overall description of what your
## functions do

## ASSIGNMENT 2. Tadimeti Keshav. raperswil@gmail.com
## The assugnment is to create a function makeCacheMatrix.
## It creates an object that will store a matrix and its inverse
## the cacheSolve function calculates an inverse if the inverse is not stored already
## ASSUMPTIONS : the input to the makeCacheMatrix function will be invertible matrix.
##
##
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y # assume matrix is always invertible
                m <<- NULL
        }
        get <- function() x
        setInv <- function(mInv) m <<- mInv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if( (!is.null(m)) && (identical(x,x$get()) ) ){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}
