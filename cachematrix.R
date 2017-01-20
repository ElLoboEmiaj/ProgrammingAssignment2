## this function create a list with a cached matrix

makeCacheMatrix <- function(x = matrix()) {
    
    INV <- NULL
    
    set <- function(y) {
        x <<- y
        INV <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) INV <<- solve
    
    getinverse <- function() INV
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function return the inverse matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
    INV <- x$getinverse()
    
    if(!is.null(INV)) {
        message("getting cached data")
        return(INV)
    }
    
    data <- x$get()
    
    INV <- solve(data, ...)
    
    x$setinverse(INV)
    
    INV
}
