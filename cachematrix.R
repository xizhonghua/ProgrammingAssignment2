## Put comments here that give an overall description of what your
## functions do

## Make an object that contains a matrix
## and whose inverse is cacheable

makeCacheMatrix <- function(x = matrix()) {
    
    my_inv <- NULL
    
    set <- function (matrix_y) {
        x <<- matrix_y
        my_inv <<- NULL
    }

    get <-function() { x }

    setInv <- function(inv) { my_inv <<- inv } 
    getInv <- function() { my_inv }

    ## return a list
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## solve the matrix with cacheable inverse
## if the inverse is already computed
##     return it directly
## otherwise
##    compute, cache and return the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached matrix inverse!")
        return(inv)
    }    
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
