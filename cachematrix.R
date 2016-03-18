## Put comments here that give an overall description of what your
## functions do

## In makeCacheMatrix function, we can set or get a matrix.
## In cacheSolve function, we can calculate inverse of a matrix.
## But if the inverse has already been founded, 
## it returns the inverse from cache without computing again.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {               # Set the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                # Get the matrix
  setinv <- function(inv) m <<- inv  # Set the inverse of the matrix
  getinv <- function() m             # Get the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {                #check if the inverse has already been founded
    message("getting cached data")
    return(m)                      #return the inverse from the cache
  }
  data <- x$get()
  m <- solve(data, ...)            #find the inverse
  x$setinv(m)
  m                                #return the inverse from calculation
}
