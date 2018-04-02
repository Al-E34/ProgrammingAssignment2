## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initiate inv as an object within the makeCacheMatrix environment 
  # to be used by later code in the function.
  inv <- NULL        
  # Assign the input argument to the x object in the parent environment
  # Assign the value of NULL to the inv object in the parent environment.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # retrieves x from the parent environment of makeCacheMatrix()
  get <- function() x
  # assign the input argument to the value of inv in the parent environment
  set_inverse <- function(inverse) inv <<- inverse
  # get the data object within makeCacheMatrix() object.
  get_inverse <- function() inv
  # assigns each of these functions as an element within a list(), 
  # returns it to the parent environment
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # calls the get_inverse() function on the input object
  inv <- x$get_inverse()
  # check cache is not NULL 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if !is.null(m) is FALSE, cacheSolve() gets the matrix from the input object, 
  # Inverse the object
  data <- x$get()
  inv <- solve(data, ...)
  # uses the set_inverse() function on the input object to set the inverse in the input object, 
  # and then returns the value of the inverse to the parent environment 
  x$set_inverse(inv)
  inv
}
