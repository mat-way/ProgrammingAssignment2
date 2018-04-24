## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(invM) inv <<- invM
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("Data from cache!")
                return(inv)
        }
        data <- x$get()
        if(det(data)==0) {
                message("Can't compute the inverse of matrix")
        } else {
                inv <- solve(data, ...)
                x$setInv(inv)
                inv
        }
}
