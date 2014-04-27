## A function pair that cache the inverse of a supplied matrix.
## This avoids the possibly costly operation of repeatedly
## calculating the inverse of the matrix.
##

## Given a matrix as argument, the makeCacheMatrix function returns
## a list whose elements are functions that are used to manipulate
## the matrix and its inverse.
## This list is later used as an argument to the cacheSolve function.
## inv --- variable that stores the inverse of a given matrix x
## get/getinverse --- getter-functions that return the matrix/the inverse.
## set/setinverse --- setter-functions that set the matrix/the inverse.
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the matrix pointed
## to by the list returned by the function makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not
## changed, then the cacheSolve function retrieves the inverse from
## the cache.
## It is assumed that any matrix is invertible.
## The inverse itself is found using the solve function in R.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
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

