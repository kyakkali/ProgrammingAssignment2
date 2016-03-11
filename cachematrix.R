## makeCacheMatrix enables caching a matrix for matrix inverse
## It exposes  4 methods to set, get the matrix AND to setinverse, getinverse of the matrix.
## These  methods enables calling function to set the inverse into cache when it does not exist

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatinverse <- function(matinverse) m <<- matinverse
  getmatinverse <- function() m
  list(set = set, get = get,
       setmatinverse = setmatinverse,
       getmatinverse = getmatinverse)

}


## cacheSolve function returns the inverse of a matrix.  
## The function first checks to see the matrix inverse is already available in the cache, 
## and if exists it returns the inverse matrix getting it from cache.
## if it doesnot, it will compute the inverse and sets it into cache for future use.
## this function leverages methods exposed by makeCacheMatrix function to either set the newly computed matrix 
## inverse into cache, or to retrieve the inverse from cache if it already exists.

cacheSolve <- function(x, ...) {
        ## Returning  a matrix that is the inverse of 'x'
  m <- x$getmatinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatinverse(m)
  m
}
  