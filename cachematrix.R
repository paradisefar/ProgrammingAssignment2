##This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  #set the value of the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #get the value of the matrix
  get<-function() x
  #set the value of the inverse
  setmatrix<-function(solve) m<<- solve
  #get the value of the inverse
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  #if previously cached, will return cached inverse
  if(!is.null(m)){      
    message("getting cached data")
    return(m)
  }
  #if not previously cached, set the inverse
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
