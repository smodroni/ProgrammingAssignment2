makeCacheMatrix <- function( m = matrix() ) {
  ## 1.Init the inverse variable
  inv <- NULL
  ## 2.set the matrix
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }
  
  ## 3.Get the matrix
  get <- function() {
    m
  }
  ## 4.Set the inverted matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  ## 5.Get the inverted matrix
  getInverse <- function() {
    inv
  }
  ## Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Calculate the inverse of a matrix or return the previously
## cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(m)
  ## Return the matrix
  m
}
