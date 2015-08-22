## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # this is where the result of inversion is stored
    set <- function(y) {
         x <<- y
         inv <<- NULL #initialise inv to null
     }
     get <- function() x # return the input matrix
     setInv <- function(solve) inv <<- solve # set the inversed matrix
     getInv <- function() inv  # return the inversed matrix
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv() # get the inversed matrix from object x
        if(!is.null(inv)) {# if the inversion result is there
         message("getting cached data")
         return(inv)
      }
      data <- x$get() # if not, get the matrix object
      inv <- solve(data, ...) # inverse the matrix
      x$setInv(inv) # set to the object
      inv # return the result
}
