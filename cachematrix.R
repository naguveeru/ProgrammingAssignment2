## This functions here are for computing and caching the inverse of the matrix
## makeCacheMatrix : This will function will create a object which has list of 4 functions - 
##      1. set(y)    = to set the matrix whose inverse needs to be cached. Input 'y' is the original matrix.
##      2. get()     = to get the matrix whose inverse is cached. Return is a matrix object.
##      3. setinv(y) = to set the inverse of the matrix x. input 'y' is the inverse matrix that is supplied as input.
##      4. getinv()  = to get the inverse of the matrix if cached. Retrun is a metrix object.
##   If you try to set the same matrix again, this function will print a message that matrix is already set. 
##   This will avoid resetting the inverse matrix.

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

## cacheSolve   : this function will take the object of type makeCacheMatrix and compute and cache the matrix inverse. 
##                If the matrix inverse is already cached, it will be fetcehd from cached and not computed again.
##   Exception- If the supplied matrix is not invertible, the function will print error.
##   Return - if the matrix is invertible , function returns the inverse matrix. Ohterwise NULL.
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getinv())) {
            print('Inverse of this matrix exists, fetching from cache')
        return(x$getinv())
        }
        data <- x$get()
        if(is.na(x$get()[1,1])) {
            print('Matrix not set. First set the matrix using x$set(<matrix>)')
            return (NULL)
        } 
        n<-tryCatch(
                 solve(data)
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
