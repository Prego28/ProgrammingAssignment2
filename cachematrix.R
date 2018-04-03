## return:a list containing functions to 
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse

## make Cache Matrix

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}


## Cache Solve

cacheSolve <- function(x, ...) {
        ## Keep in mind
        ## Inverse of the original matrix input to make Cache Matrix
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data,...)
  x$setinv(inv)
  return(inv)
}

