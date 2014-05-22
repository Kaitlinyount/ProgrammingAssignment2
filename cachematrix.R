## The following makeCacheMatrix and cacheSolve functions store the inverse of 
## a matrix in another environment so that it can be recalled in the current 
## environment. 

## To do that, the makeCacheMatrix function creates a list of four functions:
## - set: this funcion sets x and empties i.
## - get: this function returns the value of x, a matrix object
## - setinverse: this function calculates the inverse of x and stores the 
##              output in i
## - getinverse: this function returns the value of i, which is the inverse of x
 
## Then, cacheSolve can use the list elements in makeCacheMatrix as needed:
## - first it pulls in the value of i from makeCacheMatrix, 
## - if a value is returned then that value is the output of cacheSolve,
## - if no value is returned, then the inverse of the matrix x is calculated
##   and stored as i in both the makeCacheMatrix environment and the cacheSolve 
##   environment. 


## The makeCacheMatrix function creates a list of four functions that can be
## used by the cacheSolve function:

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function pulls in the cached value from makeCacheMatrix and 
## returns that value if available or otherwise calculates the inverse of x and
## stores it in makeCacheMatrix:

cacheSolve <- function(x, ...) {
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