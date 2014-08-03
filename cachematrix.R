## This function calculates inverse of a matrix using the solve function for square matrices 
## It calls the cacheSolve function to return the inverse of a matrix either from cache or 
## calculated using the solve function
## 
## the function itself returns a list containing function to set, get value of the matrix 
## and calculate the inverse of a matrix and set it in cache or return the value from cache

makeCacheMatrix <- function(x = matrix()) {
  minv <- matrix();
  
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(minv) x <<- solve
  getinv <- function() minv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculate and return the inverse of a square matrix which is crated using makeCacheMatrix
## The functions checks if the inverse of the supplied matrix is already cahched and the 
## base matrix is not changed and if so the function will return the cached inverse else it 
## will use the solve function to calculate inverse of the matrix and store it in cache and 
## return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  
  # check to see if the inverse already exists in the cache
  # if the inverse is already cached then return the cached matrix (minv)
  
  if(!is.null(minv)) {
    message("getting cache data")
    return(minv)
  }
  
  # calculate inverse of the matrix and set the inverse in cache
  
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  
}