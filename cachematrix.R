## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  #set the inv obect as NULL
  inv <- NULL
  
  #create a function to set the inv object
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #create a function to get the inv object
  get <- function() x
  
  #create a function to set the inv object to be the inverse matrix
  setinv <- function(inverse) inv <<- inverse
  
  #create a function to get the inverse matrix
  getinv <- function() inv
  
  #create a list with the four functions created
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #get the matrix
  inv <- x$getinv()
  
  # check if the inverse matrix has already been calculated
  #and if so, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #set the original matrix as the input
  data <- x$get()
  
  #calculate the inverse matrix
  inv <- solve(data, ...)
  
  #set the inverse of the matrix
  x$setinv(inv)
  
  #return the inverse matrix
  inv
}
