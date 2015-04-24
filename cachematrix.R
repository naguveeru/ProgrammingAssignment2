## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invmatrix = NULL
    set<- function(y) { if((is.matrix(x)==TRUE) &&  (all.equal(y,x))==TRUE) {
                                print('This matrix is already set') }
                        else { invmatrix <<- NULL; x <<- y }  
                      }
    get <- function() {return(x)}
    setinv <- function(y){invmatrix <<- y}
    getinv <-function() {invmatrix}
    list(set=set, get=get,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getinv())) {
            print('Inverse of this matrix exists, fetching from cache')
        return(x$getinv())
        }
        n<-tryCatch(
                 solve(x$get())
                 ,warning=function(x){'Error'}
                 ,error=function(x){'Error'}
                 ,finally={""}
        )
        if(!is.matrix(n) && n == "Error") {
                print  ('Error inverting, check if invertible matrix')  
                return (NULL)
        }
        x$setinv(n)
        return (n)
}
