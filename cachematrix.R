## Functions are used to create enhanced way to calculate inverse
## of matrix. Optimization to enhance performance is to cashes values
## of inverse matrix, if the value was already calculated. 

## Function makeCacheMatrix creates matrix that is cashed when matrix
## has already calculated. If the inverse of matrix has already
## calculated than cashed value is returned. If the new matrix
## is provided than function calculates inverse and cashes result
## and then provides result.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function uses previously created inverse
## matrix result if it exists. In case it exists, then uses
## cashed result. If the result does not exist, it uses makeCacheMatrix
## to solve inverse of matrix and return result. Not existing result 
## is cashed and prepared for next use of the function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inversed data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
