## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix

## Creates a special "matrix" object that can cache its inverse. Test.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() {x}
        setInverse<-function() {inv}
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if(!is.null(inv)){
                message("retrieval of cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat, ...)
        x$setInverse(inv)
        inv
}
