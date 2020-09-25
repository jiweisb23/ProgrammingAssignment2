## My functions create a special matrix which allow you to calculate & then cache the matrix and it's inverse


## This function creates the special matrix which stores functions allowing you to interact with cached objects

makeCacheMatrix <- function(mat = matrix()) {
  i<-NULL
  set<-function(y){
    mat<<-y
    i<<-NULL
  }
  get<-function()mat
  setinv<-function(solve)i <<-solve
  getinv<-function()i
  list(set = set, get = get,
       setinv=setinv,
       getinv=getinv)
}


## This function creates the cached inverse of your matrix if you haven't already created it

cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-mat$getinv()
  
  #I<-diag(1,nrow=dim(mat$get())[1], ncol=dim(mat$get())[2])
  
  #Check if null
  if(!is.null(i) ){
    message("getting cached data")
    return(i)
  }
  #if inv null then better calculate it
  data<-mat$get()
  i<-solve(data,...)
  mat$setinv(i)
  i
}
