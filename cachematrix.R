## makeCacheMatrix and cacheSolve: 
## A pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

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
# cache = makeCacheMatrix()
#
# myMatrix=matrix(c(4,2,6,8),2,2) # arbitrary 2x2 matrix
# cache$set(myMatrix)
# myMatrix_Inv = cacheSolve(cache)
# myMatrix_Inv %*% myMatrix # multiply original by its inverse - always get matrix I below
#
#       [,1] [,2]
# [1,]    1    0
# [2,]    0    1

## make sure it gets cached data when called a second time (check that it prints the message)
# myMatrix_Inv = cacheSolve(cache)
# getting cached data

## change the matrix, make sure it recomputes and recaches the new inverse
# newMatrix=matrix(c(3,4,2,5),2,2)
# cache$set(newMatrix)
# newMatrix_Inv = cacheSolve(cache)




