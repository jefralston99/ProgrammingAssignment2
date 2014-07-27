## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix input (assumed to be a square matrix)
## it returns a list of 4 items: set, get, setsolvedx, and getinv_x
## The list values are used in the cachesolve() function

## m is a matrix whose lexical scope is seen by both functions
## due to the use of the <<- assignment operator
## setsolvedx is the cached storage of the inverse of the input matrix
## getinv_x is the instruction to recompute the inverse of input matrix
## set and get do just as they describe

makeCacheMatrix <- function(x = matrix()) {
    
    
        m <- NULL  #initializing m to contain no values
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolvedx <- function(solve) m <<- solve
        getinv_x <- function() m
        list(set = set, get = get,
             setsolvedx = setsolvedx,
             getinv_x = getinv_x)
    

}
 

## Write a short comment describing this function
## cacheSolve returns the cached inverse of x unless it is NULL
## in which case it computes the inverse of x
## and stores it for future cache uses

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        m <- x$getinv_x()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolvedx(m)
        m
    
}
