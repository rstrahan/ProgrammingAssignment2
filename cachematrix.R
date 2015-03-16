## makeCacheMatrix and cacheSolve: 
## A pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## Usage:
## Initialize matrix object, eg: cache=makeCacheMatrix()
## 'set' method to assign a new matrix into the cache, eg: cache$set(myMatrix)
## 'get' method to retrieve cached matrix, eg cache$get()
## 'setInverse' and 'getInverse' methods are used by cacheSolve() function below.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Usage:
##  invMatrix=cacheSolve(cache)


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i    
}

####################
## Tests / Examples 
####################

## Initialize matrix cache object
# cache = makeCacheMatrix()
#
## create an arbitrary 2x2 matrix for testing
# myMatrix=matrix(c(4,2,6,8),2,2) 

## Set the value into the matrix cache, using the 'set' method
# cache$set(myMatrix)

## Find the inverse of our test matrix using the cacheSolve function
# myMatrix_Inv = cacheSolve(cache)

## test if we got a true inverse by multiplying the
## original by its inverse - should always get the IDENTITY matrix below
#
# myMatrix_Inv %*% myMatrix
#
#       [,1] [,2]
# [1,]    1    0
# [2,]    0    1

## can get both the original and inverse matrices from the cache object
# o <- cache$get()
# i <- cache$getinverse()
# o %*% i
#
#       [,1] [,2]
# [1,]    1    0
# [2,]    0    1


## Make sure cacheSolve() retrieves the cached data when called a second time 
## (check that it prints the message "getting cached data")
# myMatrix_Inv = cacheSolve(cache)
# getting cached data

## Change the matrix data, and verify the cacheSolve() recomputes and 
## recaches the new inverse
# newMatrix=matrix(c(3,4,2,5),2,2)
# cache$set(newMatrix)
# newMatrix_Inv = cacheSolve(cache)
# newMatrix_Inv %*% newMatrix
#
#       [,1] [,2]
# [1,]    1    0
# [2,]    0    1



