## The following functions will compute the inverse of a matrix and 
## cache the inverted matrix to eliminate the need to repeatedly compute it.

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
                  x <<- y
                  inv <<-NULL
          }
          get <- function() x
          setinv <- function(solve) inv <<- solve
          getinv <- function() inv
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix.  If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
