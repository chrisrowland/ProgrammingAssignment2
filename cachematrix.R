## These functions take a matrix and then calculate and return the inverse.
## Once the inverse has been calculated the result is cached in the "makeCacheMatrix" function.
## If the cacheSolve function trys to calculate the inverse again and the original matrix hasnt changed,
## then the function will use the cached inverse rather than calculating again.

## I have commented extensively to make easier to follow.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## first clear out any existing content of i (this is where we will store the inverse when we calculate it)
  i <- NULL
  
  ## define the "set" function, which sets the value of x in the main function (makeCacheMatrix)
  ## to the input y, and the value of i to nothing.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Return the value of x from the main function (makeCacheMatrix)
  get <- function() x
  
  ## setsolve is a function that takes the input "inv" and sets i (in the main function,makeCacheMatrix)
  ## equal to this.
  setsolve <- function(inv) i <<- inv
  
  ## Returns the i value from the main function
  getsolve <- function() i
  
  ## Store our functions in a list, with the function name as the list item
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## Call getinv from the list in the special vector we created, now passed to this function as x.
  ## Store the output as i.
  i <- x$getsolve()
  
  ## If i is not a null (ie it has a value) then tell user you are getting the cached inverse matrix, return it
  ## and exit this function (because we have returned something)
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If we didn't return something it means the cache was empty. So we need to pull the input matrix
  ## from the special matrix (created by makeCacheMatrix) and assign this to "data"
  data <- x$get()
  
  # Take the inverse of the matrix stored in data (with the ellipses if passed to the main function. Assign this to i.
  i <- solve(data)
  
  # Use the setinv function from the makeCacheMatrix to store i (the inverse we just calculated in this function)
  # as i in  makeCacheMatrix
  x$setsolve(i)
  
  # Return i (the inverse matrix) to the user
  i
  
}
