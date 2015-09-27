## Together, the functions makeCacheMatrix and cacheSolve can get
##or set the value of a matrix, as well as cache the inverse of the 
##matrix and return the inverse from the cache

##function makeCacheMatrix returns a list of
## the following functions for handling a matrix:
##
## set: a function to set the value of the matrix
## get: a function to return the current value of the matrix
## setinv: a function to set the value of the matrix inverse
## getinv: a function to return the inverse of the current matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<- function(y){
                x<<-y
                m<<-NULL
        }
        get<- function() x
        setinv<-function(inverse) inv<<- inverse
        getinv<-function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## function cacheSolve takes as input the list of functions returned from 
##function makeCacheMatrix.  cacheSolve returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv<- x$getinv()
        if(!is.null(inv)){
                message("Getting cached data...")
                return(inv)
        }
        data<-x$get()
        inv<- solve(data, ...)
        x$setinv(inv)
        inv
}
