## Creating a "special matrix" list that caches its inverse value
makeCacheMatrix <- function(a = matrix()) {
        inverse = NULL
        setmatrix = function(b) {
                a <<- b
                inverse <<- NULL
        }
        getmatrix = function() a
        setinverse = function(inverse1) inv <<- inverse1
        getinverse = function() inverse
list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)                    ## return the list containing functions to: set the matrix, get the matrix, set the inverse, get the inverse
}

## Function bellow checks cache and returns inverse from cache if it's already calcualted, otherwise calculates invers and returns invers.
## Previous list is used as the input to below function
cacheSolve <- function(a, ...) {
        inverse = a$getinverse()
        if (!is.null(inverse)){
                return(inverse)
        }
        mat.data = a$get()
        inverse = solve(mat.data, ...)                                                                          ## otherwise, calculates the inverse
        a$setinverse(inverse)                                                                                   ## saves inverse value in the cache
        return(inverse)                                                                                         ## Return a matrix that is the inverse of 'x'
}
