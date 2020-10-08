##There're two functions- makeCacheMatrix and cacheSolve
##In the first function I used set, get, setInv, getInv

makeCacheMatrix <- function(x = matrix()) {
             inv <- NULL    #Initialize inverse as NULL
             set <- function(y) {
                 x <<- y
                 inv <<- NULL
             }
             get <- function() {x}     #the function to get a matrix x
             setInverse <- function(inverse) {inv <<- inverse}
             getInverse <- function() {inv}
             list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
             
}

#This function is for getting the cache data

cacheSolve <- function(x, ...) {   
  inv <- x$getInverse()
  if(!is.null(inv)) {    #for checking NULL inverse
    message("getting cached data")
    return(inv)          #returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)  #calculates inv values
  x$setInverse(inv) 
  inv    #returns a matrix that is inverse of x
}