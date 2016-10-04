## The first function stores the data and the second function manipulates the data

## This function accepts the the matrix input, stores the inverse, and provides functions to edit and retrieve the inverse

makeCacheMatrix <- function(x = matrix()) {       ##instantiates the matrix a object that can cache its inverse
     
     i <- NULL                                    ##instantiates the inverse to null
     
     set <- function(y){                          ##function to change the value of x
          x <<- y                                 ##assigns a new value to x in the parent environment
          i <<- NULL                              ##clears the inverse value
     }
     get <- function() x                          ##returns x
     
     setInverse <- function(z){                   ##function to cache the inverse of x
          i <<- z                                 ##assigns a value to the inverse contained in the parent environment
     }
     getInverse <- function() i                   ##returns the inversex
     
     list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)                ##return operable functions of matrix object
     
}


## This function returns the inverse if the value already exists, and it calculates the inverse and stores it in the first
## function if not.

cacheSolve <- function(x, ...) {
     m <- x$getInverse()                          ##retrieves the cached value of the inverse
     if(!is.null(m)){                             ##skip if inverse is null
          message("getting cached data")
          return(m)                               ##return inverse and exits function
     }                                            
     data <- x$get()                              ##retrieve matrix to be inverted
     m <- solve(data, ...)                        ##inverts matrix
     x$setInverse(m)                              ##caches inverse
     m                                            ##returns inverse
}

