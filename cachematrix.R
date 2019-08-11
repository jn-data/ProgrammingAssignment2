## The two functions below can be used in conjunction to calculate the inverse of a matrix (say m1) or to 
## use the cached value of the matrix inverse if it has already been calculated.


## Usage: Assign the makeCachMatrix function to a variable e.g. mlist <- makeCacheMatrix()
## 	  then assuming matrix to invert is stored in m1 then mlist$set(m1)
##	  would be used to set the matrix within the function environment. Also mlist$get() would return 
##	  the stored matrix (m1).
##	  Running cacheSolve(mlist) would then either compute and set the inverse or output the cached
##	  copy of the inverse matrix. Thereafter mlist$getinv() would output the inverse matrix for the current stored matrix.


## The makeCacheMatrix function returns a list of functions to internally store/return a matrix and and its inverse for the use
## of the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse_mx) inv <<- inverse_mx
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The cacheSolve function below takes as input the output list from the makeCacheMatrix.
## It will check if there is already a cached copy of the matrix inverse and if not will compute it or otherwise return the
## already cached copy of the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}
