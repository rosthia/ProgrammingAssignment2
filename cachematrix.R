## This function creates a special "matrix" object that can cache its inverse.
## The function contains a list that
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the cache value of the inverse matrix
## 4. get the cache value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
##set value of the matrix
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
##get value of the maxtrix
  get <- function() x
##set the cache value of the inverse matrix
  setInverse <- function(Inverse) m <<- Inverse
##get the cache value of the inverse matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

