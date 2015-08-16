## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: constructor function which provides access to the functions required for 
## getting/setting the inverse of a matrix
## It takes a matrix and returns a list containing these get/set functions which cacheSolve uses

makeCacheMatrix <- function(x = matrix()) {

    # 'inv' will hold the inverse of matrix 'x'
    inv <- NULL
    
    # set function to (re)set the value of the matrix to the supplied parameter
    # need to reset the value of the inverse to NULL also (no longer cached)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get function to return the matrix itself
    get <- function() x
    
    # update the current value of the inverse matrix
    setinv <- function(inverse) inv <<- inverse
    
    # return the value of the inverse matrix
    getinv <- function() inv
    
    # make a list of the above functions for handling the matrix
    # this will be the return value of the function as it is the last expression
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: computes the inverse of a matrix and caches the result
## if there is *already* a cached result then this will be returned instead

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # get the stored inverse matrix
    # if it is uncached then NULL will be returned and it will have to be (re)calculated below
    inv <- x$getinv()
    if (!is.null(inv)) {
        # we have cached value so can simply return it
        message("getting cached inverse")
        return(inv)
    }
    # since inverse matrix is NOT cached, we will have to calculate it using 'solve()'
    # first get the stored matrix itself
    data <- x$get()
    
    # we can assume that the matrix is invertible, so just call solve(x) directly to get the inverse
    # any additional args from the calling function are passed on to solve()
    inv <- solve(x, ...)
    
    #update the stored value
    x$setinv(inv)
    
    # return (implicitly) this new stored value
    inv
}

