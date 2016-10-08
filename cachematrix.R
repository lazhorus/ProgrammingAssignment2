## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix makes a special "matrix" that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) {
         x <<- y
         m <<- NULL
    }
    
    get <- function () x
    setinverse <- function(solve) m <<- solve
    getinverse <- function () m
    list(set = set, get = get,
         setinverse=setinverse, 
         getinverse = getinverse )

}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x <- m$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data,...)
    x$setinvers(m)
    m
}
