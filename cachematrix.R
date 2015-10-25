## makeCacheMatrix function converts matrix into cache format , we need to pass square matrix to the function
## After makeCacheMatrix , cacheSolve function gives inverse of function from cache if exists , else computes inverse



# we need to pass a square matrix into the function
#z <- matrix(1:9,nrow=2,ncol=2,byrow=T)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The below function returns invrese 

cacheSolve <- function(x) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  } 
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
  
}


#x <- makeCacheMatrix(z)
#cacheSolve(x)