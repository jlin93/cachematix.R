## Assignment:
# Matrix inversion is usually a costly computation and there 
# may be some benefit to caching the inverse of a matrix rather 
# than compute it repeatedly (there are also alternatives to 
# matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache 
# the inverse of a matrix.

## makeCacheMatrix:
# This function creates a special "matrix" object 
# that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setmatrixinv<-function(inverse)inv<<-inverse
  getmatrixinv<-function()inv
  list(set=set, get=get,
       setmatrixinv=setmatrixinv,
       getmatrixinv=getmatrixinv)
}

## cacheSolve: 
# This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve<-function(x,...){
  inv<-x$getmatrixinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data<-x$get()
  inv<-solve(mat.data,...)
  x$setmatrixinv(inv)
  return(inv)
}

## Testing
# setting pseudo-random value to the matrix
set.seed(1110201)
r=rnorm(9)
matrix1=matrix(r,nrow=3,ncol=3)
m=makeCacheMatrix(matrix1)
m$get()
## Result from m$get():
# [,1]       [,2]       [,3]
# [1,] 0.8674016 -0.7778879 -1.6609665
# [2,] 1.5184709 -0.5377729  0.1741399
# [3,] 1.8223954 -1.5031869 -0.7346240
cacheSolve(m)
## Result from cacheSolve(m):
# [,1]        [,2]       [,3]
# [1,]  0.4058097  1.18950954 -0.6355567
# [2,]  0.8852686  1.47645400 -1.6515822
# [3,] -0.8047362 -0.07028035  0.4415881