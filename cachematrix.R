## -----------------------------------------------------------------------------
## Put comments here that give an overall description of what your
## functions do
## -----------------------------------------------------------------------------
## This second programming assignment will require you to write an R
## function that is able to cache potentially time-consuming computations.
## For example, taking the mean of a numeric vector is typically a fast
## operation. However, for a very long vector, it may take too long to
## compute the mean, especially if it has to be computed repeatedly (e.g.
## in a loop). If the contents of a vector are not changing, it may make
## sense to cache the value of the mean so that when we need it again, it
## can be looked up in the cache rather than recomputed. In this
## Programming Assignment you will take advantage of the scoping rules of
## the R language and how they can be manipulated to preserve state inside
## of an R object.
## -----------------------------------------------------------------------------
## The first function, `makeVector` creates a special "vector", which is
## really a list containing a function to:
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean
## -----------------------------------------------------------------------------
## 
## makeVector <- function(x = numeric()) {
##         m <- NULL
##         set <- function(y) {
##                 x <<- y
##                 m <<- NULL
##         }
##         get <- function() x
##         setmean <- function(mean) m <<- mean
##         getmean <- function() m
##         list(set = set, get = get,
##              setmean = setmean,
##              getmean = getmean)
## }
## 
## -----------------------------------------------------------------------------
## The following function calculates the mean of the special "vector"
## created with the above function. However, it first checks to see if the
## mean has already been calculated. If so, it `get`s the mean from the
## cache and skips the computation. Otherwise, it calculates the mean of
## the data and sets the value of the mean in the cache via the `setmean`
## function.
## -----------------------------------------------------------------------------
## 
## cachemean <- function(x, ...) {
##         m <- x$getmean()
##         if(!is.null(m)) {
##                 message("getting cached data")
##                 return(m)
##         }
##         data <- x$get()
##         m <- mean(data, ...)
##         x$setmean(m)
##         m
## }
## 
## -----------------------------------------------------------------------------
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.
## -----------------------------------------------------------------------------
## Write a short comment describing this function
## -----------------------------------------------------------------------------
## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## -----------------------------------------------------------------------------
## Computing the inverse of a square matrix can be done with the `solve`
## function in R. For example, if `X` is a square invertible matrix, then
## `solve(X)` returns its inverse.
## -----------------------------------------------------------------------------
## For this assignment, assume that the matrix supplied is always
## invertible.
## -----------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

        ## ---------------------------------------------------------------------
        ## Function to test for unchanged matrix -- from the following URL:
        ## https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
        ## ---------------------------------------------------------------------

        matrixequal <- function(a, b) {
                is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
        }

        ## ---------------------------------------------------------------------
        ## Define the functions [set], [get], [setinv], and [getinv]
        ## ---------------------------------------------------------------------

        xinv <- matrix()
        xinv <- NULL
        set <- function(y) {
                if(!matrixequal(x, y)) {
                        x <<- y
                        xinv <<- NULL
                }
        }
        get <- function() x
        setinv <- function(solve) xinv <<- solve
        getinv <- function() xinv
        
        ## ---------------------------------------------------------------------
        ## Define the list returned by [makeCacheMatrix]
        ## ---------------------------------------------------------------------
        
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## -----------------------------------------------------------------------------
## Write a short comment describing this function
## -----------------------------------------------------------------------------
## 2.  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## -----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}

## -----------------------------------------------------------------------------
