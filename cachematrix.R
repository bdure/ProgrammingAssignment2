## These functions define something so complicated that we need to cache it, apparently

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
## Per Lester, the first two lines initialize our variables, and the "set" lines here do the same
## in a new environment
  
  get <- function() x
  setinvrs <- function(solve) m <<- solve
  getinvrs <- function() m
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinvrs()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvrs(m)
  m
}

## Gee, I wish I knew linear algebra