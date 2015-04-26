# Following code presents a complex of two functions that use
# scoping rules to cache a result of potentially resource-
# intensive computation

# makeCacheMatrix - a function that creates a special "matrix"
# object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL  # Erase an old result
   set <- function(y) {
      x <<- y  # Store the last result in a cashe
      m <<- NULL
   }
   get <- function() x
   setinverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
   getinverseMatrix <- function() m
   list(set = set, get = get,
        setinverseMatrix = setinverseMatrix,
        getinverseMatrix = getinverseMatrix)
}


# cacheSolve - a function that computes the inverse of the "matrix"
# object returned by makeCacheMatrix. If the inverse has already
# been calculated (and the matrix has not changed), then
# the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   m <- x$getinverseMatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- (data, ...)^-1  # Calculate inverse of matrix
                        # returned stored in 'data'
   x$setinverseMatrix(m)
   m  # Return a matrix that is the inverse of 'x'
}
