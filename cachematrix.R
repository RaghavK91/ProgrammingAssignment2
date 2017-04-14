## The objective of the code is to cache the inverse of the matrix assuming that the given matrix is invertible.
## Two functions are written to achieve this objective

## The first function makeCachematrix creates a special matrix, which contains a list that contains functions to 
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function written below calculates the inverse of the matrix created using the above function. It checks to see whether the inverse is already calculated, else
## cacheSolve function 1. calculates the inverse of the matrix
## 2. displays the inverse of the matrix
## 3. caches the inverse using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
