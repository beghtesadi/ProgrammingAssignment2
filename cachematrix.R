## makeCacheMatrix gets a matrix as input and creates a list of four functions to cache inverse of the matrix, access it, 
## and change it.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  SetMatrix <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  GetMatrix <- function() x
  SetInverse <- function(minverse) inv <<- minverse
  GetInverse <- function() inv
  list(SetMatrix=SetMatrix , GetMatrix = GetMatrix, SetInverse = SetInverse,
       GetInverse = GetInverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
  inv <- x$GetInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$GetMatrix()
  inv <- solve(data)
  x$SetInverse(inv)
  inv
}

