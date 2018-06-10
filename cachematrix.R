## The function makeCacheMatrix creates a special matrix object
## which is a list of the differnt matrices. It then holds the cache
## of the inverse. The function cacheSolve() does the inverse computation
## and sets the inverse value

## makeCacheMatrix creates a special matrix object and holds
## the cache of the inverse.


makeCacheMatrix <- function(x = matrix()) {
        inverse <- matrix()
        setMatrix <- function(x1){
                x <<- x1
                inverse <<- matrix()
        }
        getMatrix <- function() x
        setInverse <- function(solve) inverse  <<- solve
        
        getInverse <-  function() inverse
        list(setMatrix = setMatrix,getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
        
}


## This function does the actual computation of the inverse. If the 
## cache exists, it simply returns the inverse, else it recomputes
## the inverse and puts it in the cache. If the determinant is 0,
## inverse cannot exist and its value is set to NA

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!any(is.na(inverse))){
                message("Getting cache inverse")
                return(inverse)
        }
        matrixPrime <- x$getMatrix()
        if(det(matrixPrime)!=0){
                inverse <- solve(matrixPrime,...)
                x$setInverse(inverse)
                inverse
        }
        else{
                print("Inverse does not exist , inverse will be set to NA")
                inverse <- matrix()
                x$setInverse(inverse)
                inverse
                
        }
        ## Return a matrix that is the inverse of 'x'
}