# cachematrix is a function that creates a special "matrix" object 
# that can cache its inverse. cacheSolve gets the inverse from the function call 
# to makeCacheMatrix or from memory, if the inverse is already stored.
#
# follow these steps to test out makeCacheMatrix and cacheSolve:
# a <- makeCacheMatrix(matrix(c(1:4),2,2))
# check by calling the get method, a$get(). Should return a matrix
# call cacheSolve(a) to get the inverse matrix
# then call cacheSolve(a) again to get the same inverse and it should say
# getting the data from the cache
#
###########################################################################

makeCacheMatrix <- function(x = matrix()) {
  # set m to null and define the set and get functions. Note that x is not 
  # in the set function so its value comes from a parent environment
   
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  
  # get and set the inverse of the matrix, m is not in the current 
  # environment so get it from a parent env.
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  
  # make a list of the functions as the last thing R does.
  # we do not say "return" specifically.
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

#
# cacheSolve gets the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.
#

cacheSolve <- function(x, ...) {
  m<-x$getmatrix() 
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
