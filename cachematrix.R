# The function takes in a matrix and calculates its inverse matrix and caching it for future use. If the inverse matrix has already been calculated, then it returns the cached value
# without having to recalculate the inverse.

# makecacheMatrix is a function that takes a matrix X and creates a special matrix so that its inverse can be cached.
# It actually returns a list of setting and getting the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL 
  }
  get <- function()x
  
  setinv <- function(solve) i <<- solve 
  getinv <- function() i
  list(set = set,get = get,
       setinv = setinv,
       getinv = getinv)
}


  # cacheSolve computes the inverse of the matrix inputted.
  # If the inverse has already been calculated and matrix has not been changed, then the cacheSolve retrieves the inverse from the cache itself.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
## Return a matrix that is the inverse of 'x'
  i
}
