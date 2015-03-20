## Put comments here that give an overall description of what your
## functions do

## function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## 1 set the value of the Matrix
## 2 get the value of the Matrix
## 3 set inverse of the Matrix
## 4 get inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, 
         get=get, 
         setinverse=setinverse,
         getinverse=getinverse)
}


## TThis function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    message("No cached data.")
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
## Verfify ...
##> x = rbind(c(1,2), c(4,6))
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]    4    6
##> cacheSolve(m)
##No cached data.
##     [,1] [,2]
##[1,]   -3  1.0
##[2,]    2 -0.5
##> cacheSolve(m)
##getting cached data.
##     [,1] [,2]
##[1,]   -3  1.0
##[2,]    2 -0.5
