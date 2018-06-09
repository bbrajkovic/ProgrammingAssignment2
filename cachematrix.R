## Creating a "special matrix" list that caches its inverse value
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)                    ## return the list containing functions to: set the matrix, get the matrix, set the inverse, get the inverse
}

## Function bellow checks cache and returns inverse from cache if it's already calcualted, otherwise calculates invers and returns invers.
## Previous list is used as the input to below function
cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)                                      ## otherwise, calculates the inverse
        x$setinv(inv)                                                   ## saves inverse value in the cache
        return(inv)                                                     ## Return a matrix that is the inverse of 'x'
}
