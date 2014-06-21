## This is part of the R course took up in course era
## 
## In this makeCacheMatrix() there are 4 functions set(), get(), setSolve(), 
##getSolve().
#Steps to solve these functions :
#m<- makeCacheMatrix( ) - creating a matrix Object
#m$set( matrix( c(0, 2, 2, 0 ), 2, 2)) - set a 2 * 2 matrix
#m$get() - prints the above matrix.
#

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() { x }
        
        setSolve <- function(solve) { m <<- solve }
        
        getSolve <- function() { m }
        
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve() finds the inverse of the matrix created above first time
#cacheSolve( m ) - find the inverse of the matrix m object
#cacheSolve (m ) - prints the same above output,but shall get it from cache.
#m$get() %*% cacheSolve(m) - to check if the returned matrix is inverse of not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getSolve()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        x$setSolve(m)
        m
        
}
