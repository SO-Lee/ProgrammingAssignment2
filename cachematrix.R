## makeCacheMatrix function set up to read variable x as a matrix
## establishes 4 sub-functions as set, get, setinverse and getinverse
## set and get sets up variables x, y and m
## setinverse reads the value of inverse determined in cacheSolve
## getinverse returns the value of m
## getinverse returns the value of m
## mat.list generates the functions set, get, setinverse and getinverse as hash values and publishes that as a global environmental variable

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  mat.list<<-list(set = set, get = get,
                  setinverse = setinverse,
                  getinverse = getinverse)
}

## cacheSolve first checks for a value in m, if not null, returns the value of m
## if null, data variable reads value of the get function as per mat.list
## solve is the function applied to data variable to return the inverse of the matrix read into x, and returns the value into m, which is then printed

cacheSolve <- function(x, ...) {
  m <- mat.list$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- mat.list$get()
  m <- solve(data, ...)
  mat.list$setinverse(m)
  m
}
