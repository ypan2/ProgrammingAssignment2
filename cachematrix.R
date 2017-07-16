## MakeCacheMatrix creates a special "matrix", which is really a list containing a 
## function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x<<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## The following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been
## calculated. If so, if gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
}
