## makeCacheMatrix - takes in a matrix, and sets up
## a cache to store a value related to the matrix
## that can be retrieved using internal getmatrix() method

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() 
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## cacheSolve - checks the cacheMatrix to see if the 
## matrix already has a stored  result.  If it 
## does it returns it from the cache and notifies the 
## user that the result came from cached data.  If not, it runs
## solve() on the matrix and stores the result in the cache.

## sample code to test the methods:
## d <- matrix(c(10,0,9,6), nrow=2, ncol=2)
## myMatrix <- makeCacheMatrix(d)
## cacheSolve(myMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
