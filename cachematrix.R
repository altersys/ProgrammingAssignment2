# R Programming Assignment 2
# Created by Yury Adamenook
#
# Usage example:
# > rm(list=ls())
# > source("cachematrix.R")
# > s <- makeCacheMatrix()
# > s$set(matrix(sample(1:9), nrow = 3, ncol = 3))
# > s$get()
# [,1] [,2] [,3]
# [1,]    8    6    7
# [2,]    9    5    2
# [3,]    3    1    4
# > cacheSolve(s)
# calculating data
# [,1]       [,2]       [,3]
# [1,] -0.23076923  0.2179487  0.2948718
# [2,]  0.38461538 -0.1410256 -0.6025641
# [3,]  0.07692308 -0.1282051  0.1794872
# > cacheSolve(s)
# getting cached data
# [,1]       [,2]       [,3]
# [1,] -0.23076923  0.2179487  0.2948718
# [2,]  0.38461538 -0.1410256 -0.6025641
# [3,]  0.07692308 -0.1282051  0.1794872

# Closure for matrix creation and operation
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}



## Function invokes solve function only if there is no
#  precalulated result available via getsolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        message("calculating data")
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}

