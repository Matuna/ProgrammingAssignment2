## The following functions caches the result of 
## the inverse of a given matrix in order to save computation resources
## if the operation is required more than once

## Creates a special object which caches a matrix object

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinv<-function(inv) m<<-inv
    getinv<-function()m
    list(set=set,get=get,
         setinv=setinv,
         getinv=getinv)

}


## Calculates the inverse of a special "matrix"
## however, it first checks to see if the inverse
## of the matrix has already been calculated

cacheSolve <- function(x, ...) {
        m<-x$getinv()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinv(m)
}
