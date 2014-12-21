## These functions provide a wrapper over an input matrix to allow
## caching of its inverse. Computing an inverse may be a costly 
## operation, so instead of directly computing an inverse, one could use
## makeCacheMatrix function to create a cacheable matrix object.
## The wrapper over the 'solve' inbuilt accepts such an enhanced, cacheable
## object containing the matrix, and allows one to cache the inverse with the matrix itself. 
## If an inverse is not already cached, the
## call to cacheSolve first computes the inverse and sets it in 
## the caching enhanced matrix object.



## This function accepts a matrix and returns a list of functions
## which act as enhancements to this matrix. A consumer should
## call $get on the return value to access this matrix, and can 
## set the matrix either by calling set on this function
## or passing it in the constructor.
## Usage example: z being a matrix
## d = makeCacheMatrix(z)
## OR 
## e = makeCacheMatrix() followed by
## e$set(z)
## param: x , a matrix which needs to be wrapped to cache its inverse
## return: wrapper over the matrix that has setters and getters for
## the object 'x' as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return a matrix that is the inverse of input matrix 'x'
## This function is a wrapper over solve() function
## that may be used to compute the inverse of a square matrix
## Usage: Assuming z is a matrix 'enhanced' by the makeCacheMatrix 
## function above
## answer <- cacheSolve(z)
## answer should contain the inverse of the matrix.
## second call to cacheSolve(z) will print the string 'getting cached data'
## to announce the inverse was not really computed but fetched
## from the cache.
## param x : wrapped matrix with getters and setters for the matrix
## and its inverse
## return: inverse of the matrix contained in x

cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
