## Matrix inversion is a usually a costly computation and caching the result 
## rather than computing it repeatedly may provide a benefit

## The makeCacheMatrix function below creates a special 'matrix object that can cache
##  it's inverse

# Define function makeCacheMatrix which creates a matrix object and additional
#   functions to use against the matrix object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL           # m assigned as NULL within primary function environment
    set <- function(y) {   # function defined within makeCacheMatrix to allow
        x <<- y            # values to be defined for the matrix object
        m <<- NULL          #NULL assigned to M in the parent function env.
    }
    get <- function() x   #another function defined within MCM to get values
    setinverse <- function(inverse) m <<- inverse  #Another function defined to
                                                   # allow the cachedSolve fnct
                                                 # to set the inverse of matrix
                                                 # based on value solved for
    getinverse <- function() m     # defines function getinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special 'matrix' returned
## by the makeCacheMatrix function above. If the inverse has already been
## calculated(and the matrix has not changed), then the cachesolve function 
##  should retrieve the inverse from the cache.
##
## Function assumes the matrix is always invertible.
##
## Computing the inverse of a square matrix can be done with the Solve function 
## in R, i.e. for if X is a square invertible matrix, solve(x) returns inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()  #Returns matrix inverse
    if(!is.null(m)) {    #if m isn't NULL, inverse is already in cache
        message("getting cached data")   
        return(m)
    }   #else gets the matrix value, solves for inverse and sets inverse value
    data <- x$get()   # uses get functn in cachematrix fun. to get matrix value
    m <- solve(data)   # uses solve to determine matrix inverse
    x$setinverse(m)     #uses m to setinverse with function from cachematrix
    m                  #prints m
}
