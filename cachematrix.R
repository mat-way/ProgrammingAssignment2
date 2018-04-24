## The functions work together and compute inverse matrix for original matrix if it's possible
## Besides if inverse matrix were computed then return computed value from cache without 
## new computations

## Set matrix for computing in argument 'x'

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


## Compute inverse matrix for original matrix 'x' and check that I can find inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("Data from cache!")
                return(inv)
        }
        data <- x$get()
        
        ## only matrix with deteminant not equal zero can have inverse matrix
        ##I check it here
        
        if(det(data)==0) {
                message("Can't compute the inverse of matrix")
        } else {
                inv <- solve(data, ...)
                x$setInv(inv)
                inv
        }
}
