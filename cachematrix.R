## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
E<-diag(nrow(x))
solve<-solve(x,E)
m <- NULL
set <- function(y) {
    x <<- y
    m <<- NULL
}
get <- function() x
setMATRIX <- function(solve) m <<- solve(x,E)
getMATRIX <- function() m
list(set = set, get = get,
     setMATRIX = setMATRIX,
     getMATRIX = getMATRIX)
}


## solve()is used to calculate inverse matrix, and I use the same function as the instruction gives

cacheSolve <- function(x, ...) {
    m <- x$getMATRIX()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(x,E)
    x$setMATRIX(m)
    m
}
    
        ## Return a matrix that is the inverse of 'x'

