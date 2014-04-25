## Functions for cached matrix inverse operation

## makeCacheMatrix is used for creation of special 
## matrix object that caches the calculated inverse
## until the matrix itself changes. 
## cacheSolve uses special matrix object x created by 
## makeCachedMatrix in order to calculate the inverse 
## of the matrix x. When the matrix inversion result 
## from previous calculation is available in the cache,
## it is just reported back without the new recalculation. 
## Otherwise, the inverse is calculated and stored in
## the cache for future reuse


## makeCacheMatrix creates special form of matrix that
## enables caching of matrix inverse calculations

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    if (!identical(x,y))
    {
        x <<- y
        inv <<- NULL
    }
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes special form of matrix as input
## and returns its inverse, either by reporting the cached 
## or calculated version

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


