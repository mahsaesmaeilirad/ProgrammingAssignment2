## Two functions, makeCacheMatrix and cacheSolve that cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function(){
    x
  }
  setInverse<-function(inverse){
    i<<-inverse
  }
  getInverse<-function(){
    i
  }
  list(set=set, get=get,
       setInverse=setInverse, getInverse=getInverse)
}


## The second matrix, cacheSolve calculate the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data) %*% data
  x$setInverse(i)
  i
}
