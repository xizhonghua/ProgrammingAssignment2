## Put comments here that give an overall description of what your
## functions do

## Make an object that contains a matrix
## and whose inverse is cacheable
## 
## Usage:
## x <- makeCacheMatrix()
## x$set(matrix(rnorm(9),3,3))
## x$get()

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
## 
## Example usage:
## x <- makeCacheMatrix(rnorm(9),3,3)
## cacheSolve(x)
## ## inverse of x is cached, no computational cost
## cacheSolve(x) 

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
