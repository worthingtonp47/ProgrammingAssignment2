## The project creates a matrix from the given inputs. It moves on the calculate the
## inverse of the matrix and store that calculation in memory. The next time the project
##is called it searches the memory for a cached value before going through the process
## of calculating the inverse matrix.

## This code is the cache, or "save to memory" piece of the project

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This piece of code is doing the heavy lifting of the actual solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  mat1 <- x$get()
  s <- solve(mat1, ...)
  x$setsolve(s)
  s
}
