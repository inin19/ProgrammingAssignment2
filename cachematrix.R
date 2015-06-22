##  This function takes in a matrix as argument and do the following 
##  set the value of the matrix
##  get the value of the matrix
##  set the inverse of the matrix
##  get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    iMatrix <- NULL 
    set <- function(y){
        x <<- y
        iMatrix <<- NULL
    }
    
    get <-function() x
    setInverse <- function(inverse) iMatrix <<- inverse
    getInverse <- function() iMatrix
    
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
    
}


## get the inverse of the matrix from cached if it's been calculated, otherwise calcualte the inveres of the matrix 
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    iMatrix <- x$getInverse()
    if(!is.null(iMatrix)){
        message("getting cached Matrix Inverse data")
        return(iMatrix)
    }
    data <- x$get()
    iMatrix <- solve(data)
    x$setInverse(iMatrix)
    iMatrix
    
}
