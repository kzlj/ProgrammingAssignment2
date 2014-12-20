## makeCacheMatrix creates object that keeps matrix 
## and caches inversed matrix, if calculated. 
## makeCacheMatrix provides also 4 functions for manipulating it.
makeCacheMatrix <- function(x = matrix()) {
    
    # set inversed matrix as null on initialization
    inversedMatrix <- NULL
    
    # set function allows to set new matrix and clear cache
    set <- function(y) {
        x <<- y
        inversedMatrix <<- NULL
    }
    
    # function returns stored matrix
    get <- function() x
    
    # sets inversed matrix in cache
    setsolve <- function(solve) inversedMatrix <<- solve
    
    # returns cached inversed matrix
    getsolve <- function() inversedMatrix
    
    # return list of functions for object manipulation
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve returns a matrix  that is the inverse of 'x'. 
## It accepts makeCacheMatrix objects.
cacheSolve <- function(x, ...) {
    
    ## check if inversed matrix is not already cached in 'x'
    inversedMatrix <- x$getsolve()
    if(!is.null(inversedMatrix)) {
        message("getting cached data")
        # return cached inversed matrix
        return(inversedMatrix)
    }
    
    # get matrix to be inversed
    data <- x$get()
    
    # perform matrix inversion
    inversedMatrix <- solve(data, ...)
    
    # store inversion result in cache
    x$setsolve(inversedMatrix)
    
    # return inversed matrix
    inversedMatrix    
}
