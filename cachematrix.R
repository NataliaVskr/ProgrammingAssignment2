##This function creates a special "matrix" object that can cache its inverse
##MakeCacheMatrix works with matrices
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##There is a place where inversion is stored
  ##This function sets specified matrix to object created by makeCacheMatrix function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##This function return the matrix
  get <- function() x 
  ##This function set the inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  ##This function return the inverse matrix
  getInverse <- function() inv
  ##List below consists of all results of functions which I mentioned before
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ##Set the inversed matrix to inv from x
  inv <- x$getInverse()
  ##If we have inversion result...
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv) ##return the calculated inversion. 
  }
  ##If we don't have then we solve inv
  forsolve <- x$get()
  inv <- solve(forsolve, ...)
  x$setInverse(inv)
  inv ##Return the solved result
}


