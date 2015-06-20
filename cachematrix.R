## These functions create a matrix object with a 
## setter, getter, and a function to compute the inverse
## of the the (possibly previously) cached matrix and then 
## cache the inverse as well.

## Constructor of the cached matrix object

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set
         , get = get
         , setinverse = setinverse
         , getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of 
## the cached matrix, if it has not yet been calculated
## If it has already been calculated and cached, it
## returns the cached inverse.

## Assumption: the matrix supplied is invertible

## Example
##  m <- matrix(c(1,0,4,1,3,4,4,1,0), nrow=3, ncol=3)
##  myMatrix <- makeCacheMatrix(m)
##  cacheSolve(myMatrix)
##              [,1]       [,2]        [,3]
##  [1,]  0.08333333 -0.3333333  0.22916667
##  [2,] -0.08333333  0.3333333  0.02083333
##  [3,]  0.25000000  0.0000000 -0.06250000

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
