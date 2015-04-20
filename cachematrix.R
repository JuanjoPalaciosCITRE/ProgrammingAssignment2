## makeCacheMatrix will create a list of functions in a selected variable
## these functions will:
## a) get - the matrix from which to obtain its inverse
## b) setinverse - calculate the inverse of the input matrix
##    and store it on the parent environment variable "m"
## c) getinverse - obtain the previously stored inverse matrix
## d) set - initial values for the matrix and clear m in order to reuse
##    the functions and make sure values are correct in both environments.

makeCacheMatrix <- function(x = matrix()) {
  # Clear m in the functions environment
  m <- NULL
  set <- function(y) {
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


## cacheSolve uses the functions in the list created with
## makeCacheMatrix. I will first test for the existance of the inverse 
## matrix loaded in m. If m is not NULL, cacheSolve will proceed to load
## the inverse from cache. Otherwise will use functions defined in
## makeCacheMatrix to calculate the inverse and store in m.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Tries to read the calculated inverse, in case it exists
  ## so the program does not calculate the inverse constantly
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    # sequence is interrupted and inverse returned in m
    return(m)
  }
  # If there's no inverse, get the matrix
  data <- x$get()
  # Calculate inverse with the function "solve"
  m <- solve(data, ...)
  # Store the inverse in m for retrieval and return the value
  x$setinverse(m)
  m  
}
