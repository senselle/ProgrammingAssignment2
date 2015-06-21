## These functions are used to calculate and cache the value of the inverse 
## of the given invertible square matrix


## Function takes a given matrix and saves its value.
## it returns a list of four functions: 
## (i) set: sets new value of the matrix
## (ii) get: gets saved matrix
## (iii) setinversed: inits the value of the inversed matrix
## (iv) getinversed: gets the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  saved_inversed_matrix <- NULL
  ## takes a new matrix and saves it in object
  ## cleans cached inversed value
  set <- function(y) {
    x <<- y
    saved_inversed_matrix <<- NULL
  }
  
  get <- function() x
  ## takes an inverse matrix and sets it to saved_inversed_matrix
  setinverse <- function(inverse) saved_inversed_matrix <<- inverse 
  getinverse <- function() saved_inversed_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve calculates the inverse of the matrix 
## created with the function makeCacheMatrix. 
## If the inverse of the matrix has already been calculated 
## the function skips the computation and returns it's value.
## Otherwise it calculates the inverse of the matrix,
## saves it's value to cache and returns the inversed matrix

cacheSolve <- function(x, ...) {
  inversed_matrix <- x$getinverse()
  
  if(!is.null(inversed_matrix)) {
    message("getting cached data")
    return(inversed_matrix) 
  }
  data <- x$get()
  inversed_matrix <- solve(data, ...)
  x$setinverse(inversed_matrix)
  inversed_matrix
}