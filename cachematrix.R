## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve funtion finds a inverse of special "matrix" produced by makeCacheMatrix function
## if the inverse matrix was calculated before, 
## then this returns the inverse matrix from the cache that saved the previous result
## otherwise it calculates inverse matrix by using 'solve' function, 
## stores it in the cache and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
