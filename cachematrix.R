## makeCacheMatrix will generate a matrix and cache its inverse, while cacheSolve checks to see if the inverse has already been calculated.
## If it has it will pull it from the cache, and if not it will calculate the inverse.

## This function generates the starting matrix, creates the cache, and defines the get, setinverse, and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
  inv.matrix <- NULL
  set <- function(y) {
    x <<- y
    inv.matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv.matrix <<- inv
  getinverse <- function() inv.matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the inverse has been calculated already. If it has, it will return the inverse from the cache with a message. If it hasn't 
## it will calculate the inverse and use the setinverse function from the makeCacheMatrix environment to load the inverse of the matrix to the environment.

cacheSolve <- function(x, ...) {
    inv.matrix <- x$getinverse()
    if(!is.null(inv.matrix)) {
      message("getting cached data")
      return(inv.matrix)
    }
    data <- x$get()
    inv.matrix <- solve(data, ...)
    x$setinverse(inv.matrix)
    inv.matrix
        ## Return a matrix that is the inverse of 'x'
}
