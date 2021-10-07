## Set the input x as a matrix
## First part of code is for caching and assigning the cache name

##The assignment is for caching and inversing
##First part is for caching

makeCacheMatrix <- function(x = matrix()) {
  ##Special matrix created that caches its inverse is made with this function
  invrs <- NULL 
  ##Initializes invz as Null
  ##Initializes hold value of matrix Inverse
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  get <- function()x{     ##define the matrix func. as get
  setInverse <- function(inverse) invrs <<- inverse ##inverse set as invrs
  getInverse <- function() invrs{ ## gets the inverse value
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## Returning matrix from an inverse of x
cacheSolve <- function(x, ...) {
  ## Matrix return that is an inverse of x
  j <- x$getInverse()
  if(!is.null(invrs)){
    message("Fetching cached data please wait")
    return(invrs)
  }
  D <- x$get()
  invrs <- solve(D,...)
  x$setInverse(invrs)
  invrs
}
