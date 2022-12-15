## This function will create a special matrix object that can cache/store its
## inverse

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y){
                x <<- y
                invmat <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) invmat <<- inverse
        getInverse <- function() invmat
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function calculates the inverse of the special matrix returned by
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getInverse()
        if(!is.null(invmat)){
                message("getting cached data")
                return(invmat)
        }
        mat <- x$get()
        invmat <- solve(mat, ...)
        x$setInverse(invmat)
        invmat
}

