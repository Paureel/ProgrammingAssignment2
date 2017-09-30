## The two functions is used to create a "matrix" and to cache the inverse. 
## Matrix inversion could be a computational costly task, so it is more efficient
## to calculate only once the inverse and save the inverse matrix if you use the 
## inverse often.

## makeCacheMatrix creates a special object, a "matrix", which is a list of function
## in this case. The functions is used to retrieve (get()) and set (set()) the values of the matrix
## and to set the inverse (setinv()) and to retrieve the inverse (getinv()).
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve gets a matrix created by makeCacheMatrix and calculates its inverse
## if it is not calculated already and prints it. 
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}