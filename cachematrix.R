## This function caches the computation for the inverse of a matrix
## so that this computation does not have to be done repeatedly. 

## This function produces a list that contains functions to set and retrieve 
## a matrix and it's inverse. It uses the <<- operator to demonstrate lexical scoping.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    
    set <- function(y) {
      
      x <<- y
      
      inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(inverse) inv <<- inverse
    
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function retrieves the cached value of the inverse of a matrix,
## or it computes said inverse if it has not already been computed. 

cacheSolve <- function(x, ...) {
        
    inv <- x$getInv()
    
    if(!is.null(inv)) {
      
      message("Retrieving cached matrix.")
      
      return(inv)
    }
    
    else {
      
      data <- x$get()
      
      inv <- solve(data)
    
      x$setInv(inv)
      
      inv
    }
}
