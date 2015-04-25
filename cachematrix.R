## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## variables defined internal
    m <- NULL;
    ## <<- "for global"
    ## let x be y
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    ##return the data that are to be processed
    get <- function() x
    ##let m be the value of solve
    setinverse <- function(solve) m <<- solve
    ##return the result m
    getinverse <- function() m
    ##each element is defined above
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ##inspect whether x has been calculated before
    m <- x$getinverse()
    if(!is.null(m)){
        ##if m is not NULL, which means the inverse of x hasn't been calculated
        message("getting cached data")
        return(m)
    }
    ##get data
    data <- x$get()
    ##calculate the inverse of x
    m <- solve(data,...)
    ##return the data that has been stored
    x$setinverse(m)
    ##return the result
    m
}
