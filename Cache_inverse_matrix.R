##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## initialising the inverse matrix
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    ##Check matrix is invertible
    ## determiniant is not zero and
    ## matrix is square
    if(det(x) != 0 && nrow(x) == ncol(x)){
        setinverse <- function(solve) {
            inverse <<- solve
            ## store matrix that inverse is basedd on for checking later
            store <<- x
            }
        getinverse <- function() inverse
        getstore <- function() store
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse,
            getstore = getstore)
    }
    else {
        message("matrix is not invertible")
        return
    }
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    ## Check if cached inverse can be used
    if(!is.null(inverse) && identical(x$getstore(), x$get())) {
        message("getting cached data")
        return(inverse)
    }
    ## calulate inverse if no inverse stored or matrix has changed
    data <- x$get()
    message("calculating inverse")
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}


##Test
m1 <- matrix(c(-1,1.5,1,-1), nrow = 2, ncol = 2)
make_Mat <- makeCacheMatrix(m1)
invMat <- cacheSolve(make_Mat)
invMAT
invMAT <- cacheSolve(make_Mat)
invMat

## Compare with using solve directly
if (identical(solve(m1),invMat)) print("same answers")


## Another test
m2 <- matrix(1:4,nrow = 2, ncol = 2)
make_Mat <- makeCacheMatrix(m2)
cacheSolve(make_Mat)
cacheSolve(make_Mat)

## Test with matrix that is not invertible
m3 <- matrix(rep(1,9),nrow = 3, ncol = 3)
make_Mat <- makeCacheMatrix(m3)
cacheSolve(make_Mat)

