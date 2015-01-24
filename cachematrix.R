## Algorithm using caching for matrix inverse

## Generates a cached matrix that exposes getter/setter functions
## get / set : Matrix data
## getinverse / setinverse : Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(invmat) inverse <<- invmat
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Provides inverse of matrix
## Uses the caching option provided by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
