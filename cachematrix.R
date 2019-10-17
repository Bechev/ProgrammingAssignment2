## This works with 2 function. One function will be in charge of defining a list of function, to cache and get a matrix and it's invert.
## The second function will take a matrix as an argument and by using the set of functions used, will allow to calculate the invert of a matrix accessing cached information


makeCacheMatrix <- function(x = matrix()) {
    ## Set m as NULL
  m <- NULL
  #define the set fucntion which will set the matrix_inverse in x (caching the inverse of the matrix)
  set <- function(matrix_inverse) {
    x <<- matrix_inverse
    m <<- NULL
  }
  #Define the get function which gets the x value (getting the value cached in the set function)
  get <- function() x
  #Define the setinversve function which sets m as the inverse of the matrix
  setinverse <- function(matrix_inverse) m <<- matrix_inverse
  #Define the getinverse function which gets m which is the inverse of the matrix set by setinvers
  getinverse <- function() m
  #Define a list with the various functions defined so they can be used on the cached data 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## cacheSolve takes as argument a list of funciton that can access cached data
  ## Access the cached inverse matrix
  m <- x$getinverse()
  ## If an inverse matrix is cached, returns this inverse matrix and break the rest of the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Access the non inverse matrix and store it in data
  data <- x$get()
  ## Provides the inverse of the matrix
  m <- solve(data, ...)
  ## Cache the inverse matrix using 
  x$setinverse(m)
  ## Return the inverted matrix
  m
}


