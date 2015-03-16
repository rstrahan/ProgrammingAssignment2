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

##############
## Test case 
##############
# cache = makeCacheMatrix()
#
# myMatrix=matrix(c(4,2,6,8),2,2) # arbitrary 2x2 matrix
# cache$set(myMatrix)
# myMatrix_Inv = cacheSolve(cache)
# myMatrix_Inv %*% myMatrix # multiple original by its inverse to get 1
#
#       [,1] [,2]
# [1,]    1    0
# [2,]    0    1


