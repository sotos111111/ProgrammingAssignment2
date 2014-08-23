## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix initiates a set of listed of functions for storing and obtaining the to-be-inverted matrix (set and get) and for storing and calculating the inverse matrix (setinverse, getinverse).

makeCacheMatrix <- function(x = matrix()) {
      j <- NULL
      set <- function(y) {
            x <<- y
            j <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) j <<- solve
      getinverse <- function() j
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve calculates the inverted matrix of x. If the cache contains a previously a previously calculated inverse matrix of x then it just prints it out.

cacheSolve <- function(x, ...) {
      j <- x$getinverse()
      if(!is.null(j)) {
            message("getting cached data")
            return(j)
      }
      data <- x$get()
      j <- solve(data, ...)
      x$setinverse(j)
      j
}
