## Creating a special function that will return the inverse of a Matrix, 
## then a seperate function that will compute the inverse of the 
## special "matrix" returned by makeCacheMatrix

## Creating a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set<- function(y) {
        x<<-y
        m<<- NULL
    }
    get <- function() x
    setinverse<-function(inverse) m<<- inverse
    getinverse<-function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
          message("getting cached data")
          return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
