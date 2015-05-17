## Set of functions to compute & cache matrix and matrix inverse as computing matrix inverse is expensive

## This function creates a caching mechanism for storing a matrix and it's inverse
## Return a list that facilitates store  & retrieve matrix/inverse from cache.
makeCacheMatrix <- function(x = matrix()) {
  
  xi <- NULL
  set <- function(y) {
    #Reset matrix/inverse only if the cached matrix is changing.
    if (!(is.matrix(y) &&  all(dim(x)==dim(y)) && all(x==y)))
    {
      x <<- y
      xi <<- NULL
    }
  }
  get <- function() x
  setInv <- function(inv) xi <<- inv
  getInv<- function() xi
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
    
}


## Return inverse of the matrix from cache; Or compute, cache when not found in the case
## assume x is created by makeCacheMatrix()
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Get get the cached matrix 
  
  xi <- x$getInv()
  if(!is.null(xi)) {
    message("Found inverse in the cache")
    return(xi)
  }
  mat <- x$get()
  xi <- solve(mat, ...)
  x$setInv(xi)
  xi  
}
