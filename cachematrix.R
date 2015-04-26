## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
   getinverseMatrix <- function() m
   list(set = set, get = get,
        setinverseMatrix = setinverseMatrix,
        getinverseMatrix = getinverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   m <- x$getinverseMatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- (data, ...)^-1
   x$setinverseMatrix(m)
   m  ## Return a matrix that is the inverse of 'x'
}
