## The cachematrix is a special type for holding a square matrix and 
## a cached copy of the inverse of that matrix.
##
## Usage example:
##   cm <- makeCacheMatrix() # create an empty cachematrix
##   cm$set(matrix(c(1,1,2,1,1,4,2,-1,0),3,3)) # fill it with a new matrix
##   m <- cm$get() # retrieve a usable version of the matrix
##   m_inverse <- cacheSolve(cm) # get its inverse 
##      # (will be slow since it's the first time)
##   print(cacheSolve(cm)) # print its inverse
##      # (should be fast since the inverse was pre-calculated)


## returns a cachematrix list object with functions to store and retrieve 
## a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## returns the inverse of the matrix stored in the cachematrix type
## (computing it only when necessary)

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}



##########################################
## I would have done that all in one.
## 
## Usage example:
##   cm <- makeCacheMatrix() # create an empty cachematrix
##   cm$set(matrix(c(1,1,2,1,1,4,2,-1,0),3,3)) # fill it with a new matrix
##   m <- cm$get() # retrieve a usable version of the matrix
##   m_inverse <- cm$solve() # get its inverse 
##      # (will be slow since it's the first time)
##   print(cm$solve())  # print its inverse
##      # (should be fast since the inverse was pre-calculated)
#
# myCacheMatrix <- function(x = matrix()) {
#    inv <- NULL
#    set <- function(y) {
#        x <<- y
#        inv <<- NULL
#    }
#    get <- function() x
#    mysolve <- function(...) {
#            if(!is.null(inv)) {
#                message("getting cached data")
#                return(inv)
#            }
#            inv <<- solve(x, ...)
#            inv
#        }
#    list(set = set, get = get, solve = mysolve)
#}
