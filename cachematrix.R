## We create a new kind of matrices where we cache the inverse of each matrix
## so that it only needs to be calculated once.

# This function creates a new Cache Matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse=NULL
  set=function(y) {
    x<<-y
    inverse<<-NULL
  }
  get=function() x
  setinv = function(inv) inverse<<-inv
  getinv = function() inverse
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

# This function gets the inverse of x (calculates it if necessary)
cacheSolve <- function(x, ...) {
  inv=x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data=x$get()
  inv=solve(data,...)
  x$setinv(inv)
  inv
}