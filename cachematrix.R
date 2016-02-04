## makeCacheMatrix and cacheSolve together provide a way to
## inverse an invertible matrix and cache the result for repeated use.
##
## In order to make use of the functionality provided by the functions do this:
## 1. Pass an invertible matrix to makeCacheMatrix and assign the result to a variable, e.g.
##    y <- makeCacheMatrix(matrix(c(2, 2, 3, 2), 2, 2))
## 2. Call cacheSolve passing y (the result of the call to makeCacheMatrix) as an input parameter, e.g.
##    cacheSolve(y)
##    The result will be the inversed matrix, or an error if the matrix is not invertible.
##
## Note 1: cacheSolve will determine if there is already a cached inverse matrix in y (the input parameter)
## and if there is, then it will retrieve it as the result. In this case, it will also write the message
## "getting cached data" to the console indicating that the result came from a cached value.
## If cacheSolve determines that there is no cached inverse matrix contained in y, then it will inverse
## the matrix, store the inverse matrix in y for future use and return the inverse matrix as its result.
##
## Note 2: once a call to makeCacheMatrix has been made and the result has been stored, e.g. in the variable y
## (as shown above), one can store a new matrix into y directly by calling y$set(), e.g.
## y$set(matrix(c(2, 2, 3, 2), 2, 2))



## makeCacheMatrix: caches a matrix and its inverse matrix
##
## input: invertible matrix to be cached 
## output: list of 4 functions providing the following functionality:
##      set:        caches a new (non-inversed/to be inverted) matrix provided as an input parameter
##      get:        returns the cached (non-inversed) matrix
##      setinverse: caches the inverse matrix provided as an input parameter
##      getinverse: returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve: inverses an invertible matrix and returns the result
##
## input: list of get/set functions returned from makeCacheMatrix
## output: inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
