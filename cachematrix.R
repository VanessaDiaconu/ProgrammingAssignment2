## this function creates a list containing a function to get and set the values of the matrix and it's respective mean

makeCacheMatrix <- function(x = matrix()){
    
    # set matrix value
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # get matrix value
    get <-function() x
    
    # set matrix inverse
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    # get matrix inverse
    list(set = set, get = get,
         setinverse =setinverse,
         getinverse=getinverse)
}

# this function calculates the inverse of the matrix, 'x', created in the makeCacheMatrix function


cacheSolve <- function(x, ...){
    # get matrix inverse
    m <- x$getinverse()
    
    # check if inverse calculation is stored in cache already
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    # get matrix inverse otherwise
    data <- x$get()
    m <- solve(data, ...)
    
    # cache matrix inverse
    x$setinverse(m)
    
    # return matrix inverse
    m
}
