
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
          #  `<<-`  assign a value to an object in a diffrent environment from the current one. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##  cacheSolve return inverse of the original matrix input to makeCacheMatrix()


cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)){
          ## chech for the the data in the cached- if so returns it
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
        ## inverse the matrix
  x$setinv(inv)
  return(inv)
}
