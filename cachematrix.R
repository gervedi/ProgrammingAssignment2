## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

##Function that save a inverse solution matrix into the cache if it does not exist. 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(z){
    x <<- z
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv
  
  list(set = set, get = get, 
       setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

##Function that solve the inverse matrix if it is not on the cache,
##otherwise, found the solution on the cache. Returns the inverse of the given matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  if(!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  
}

