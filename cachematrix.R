## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Week 3 Assignment
## This function creates a matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                             ## holds value of matrix inverse 
        set <- function(y){
                x <<- y
                inv <<- NULL                                    ## reset inv to NULL if there is a new matrix 
        }
        get <- function() x
        
        setInverse <- function(invMatrix) inv <<- invMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If matrix has not changed, then it will used the cashed inverse from the above function to 
## compute rather than resolving the matrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
