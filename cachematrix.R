## This function cache the inverse matrix of
## original matrix "x" from cache or calculate by
## ifself at the first time and cache it

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseMat <- NULL
    
    # Setter for matrix data
    set <- function (mat) {
        x <<- mat
        inverseMat <<- NULL
    }

    # Getter for matrix data
    get <- function ()  {
        x
    }

    # Setter for inverse matrix data
    setInverseMat <- function (mat) {
        inverseMat <<- mat
    }

    # Getter for inverse matrix data
    getInverseMat <- function ()  {
        return (inverseMat)
    }   

    list(
        set = set,
        get = get,
        setInverseMat = setInverseMat,
        getInverseMat = getInverseMat)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # Try to get inverse from cache
    inverseMat <- x$getInverseMat()

    if (!is.null(inverseMat)) {
        message("getting cached inverse matrix data")
        return(inverseMat)
    }

    # Calc the inverse matrix at the first time
    mat <- x$get()
    inverseMat <- solve(mat, ...)

    # Save to cache
    x$setInverseMat(inverseMat)

    inverseMat

}
