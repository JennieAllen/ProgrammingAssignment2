## This is the programming assignment for Jennie Allen for week 3 of coursera:R programming
## The following 2 functions make a cacheMatrix and cacheInverseMatrix

## This function will create a special "matrix" 
##object containing functions that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<-inverse
        getInverse <- function()m
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## This is the function that solves for the inverse of a matrix, 
##but if the inverse has already been computed 
##it will retrieve the cached value

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
