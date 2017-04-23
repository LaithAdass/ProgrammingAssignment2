## The makeCacheMatrix sets and gets the value of the matrix in addition to
## setting and getting the value of the inverst of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse)
}



## The cacheSolve funtion retruns the inverse of the matrix if it is already computed
## If the inverse of the matrix is not already computed, it computes it, sets the value in cash then returns inverse

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)){
    message("Getting cashed date")
    return(invs)
  }
    data<-x$get()
    invs<- solve(data,...)
    x$setinverse(invs)
  invs
        ## Return a matrix that is the inverse of 'x'
}
