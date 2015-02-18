#  These 2 functions do matrix inversion via solve()
#  using lexical scoping and caching function to speed up calculations
#  in loop.
#
#  Example of use:
#           source("cachematrix.R")
#           a<- makeCacheMatrix()
#           a$set(matrix(c(1,3,5,8), 2, 2))
#           cacheSolve(a)


## makeCacheMatrix
# This function creates a list of functions to set/get matrix values
# and set/get inverted matrix values (setsolve/getsolve).
# Matrix and inversed matrix, via "<<-" operator, are stored in an environment
# different to the current

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve
# This function uses functions defined in makeCacheMatrix. 
# First tries to load inverted matrix (if present). If succeeded then sends
# appropriate message and returns cached inverted matrix. 
# Otherwise loads matrix, invertes it and stores result in cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}