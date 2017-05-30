## The functions are about caching the inverse of a matrix (not having to compute
## it repeatedly) which can be useful as matrix inversion is usually a costly
## computation.

## makeCacheMatrix creates a matrix object and caches its inverse.
## makeCacheMatrix contains the functions:
  ## set (creating a matrix)
  ## get (getting the matrix)
  ## setinv (creating the inverse of the matrix)
  ## getinv (getting the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {

  # Setting cached value to NULL at the beginning
  inv <- NULL
  
  # Creating a matrix
  set <- function(y) {
      x <<- y
      inv <<- NULL
      }
  
  # Getting the matrix
  get <- function () x
  
  # Creating the inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
   
  # Getting the inverse of the matrix
  getinv <- function() inv
 
  # returning list of functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}

########################################

## cacheSolve computes the inverse of a matrix if not already cached
## (in this case it it just returns the cached data) 

cacheSolve <- function(x, ...) {
  
  # getting the cached value      
  inv <- x$getinv()
  
  # if inverse has already been calculated (is stored in cache): return the cached value 
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
    }
  
  # if not: computing and returning the inverse of the matrix
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  inv
  
}







