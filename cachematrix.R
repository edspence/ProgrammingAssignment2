## These functions cache a matrix inversion computation to save resources
## instead of recalculating constantly through a loop function like apply.

## The matrix inversion computation is done using the solve() function.

## These functions are modified from the makeVector and cachemean functions
## given as examples.

## makeCacheMatrix builds a set of functions and returns the functions within
## the parent environment, these include set, setinverse, get & getinverse.

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Due to lexical scoping rules, cacheSolve will retrieve x from the parent
## environment, (where it was defined by makeCacheMatrix), instead of 
## recalculating the inversion of x with each pass.

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
