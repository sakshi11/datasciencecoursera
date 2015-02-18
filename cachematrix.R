## The following pair of functions calculates and cache the 
## inverse of a square matrix

## This makeCacheMatrix() function creates a special "matrix"
## object that can be cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinvmatrix<-function(solve) m<<- solve
  getinvmatrix<-function() m
  list(set=set, get=get,
       setinvmatrix=setinvmatrix,
       getinvmatrix=getinvmatrix)
  
}


## This cacheSolve() function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above.  If the 
## inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve() retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
   
  m<-x$getinvmatrix()
  if(!is.null(m)){
    message("getting cached data") ## This message is displayed in case the inverse is cache
    return(m)
  }
  data <- x$get() 
  m<-solve(data, ...)
  x$setinvmatrix(m)
  m
}
