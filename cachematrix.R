## The functions below enable the computation of the inverse of a matrix and the reuse
## of the result without the need to recompute the inverse again and again.
## The first function makeCacheMatrix creates a special "matrix" and returns a list
## of four functions to set and get the matrix and its inverse. It also saves the value
## NULL into the variable inv in the parent environment. This signals that the calculation
## of the inverse was not yet done.
## The second function cacheSolve checks the value of the variable inv. If this is NULL 
## the inverse is calculated, stored in this variable for future use and  is returned to the caller.
## If the variable inv is not NULL, it is just returned to the caller.

#example of usage:
#  m <- matrix(c(4, 1, 5, 8),2,2)
#  mm <- makeCacheMatrix(m)
#  inv <- cacheSolve(mm)  
#  inv <- cacheSolve(mm)
#  In the first call of cacheSolve the inverse is computed and is also stored in the cache
#  In the second call of cacheSolve the inverse is not computed but returned from the value stored in the cache


## makeCacheMatrix create a special "matrix" that can cache its inverse. The fuction returns a list of 4 functions for setting and getting the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse ,
                 getinverse  = getinverse)

}


## cacheSolve returns the inverse of a special "matrix" either by computing it or by extracting it from the cache if it has already been computed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getinverse ()
            if(!is.null(inv)) {
        ##          message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
}
