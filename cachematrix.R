# This assignment is about solving the inverse of a matrix by caching the 
# result within a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve".

# The function "makeCacheMatrix" creates a new, unique environment. 
# The inverse matrix is cached inside the object inv, within the main 
# environment, which is unique for each instance the function is called.

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


# The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function, for ex. x$getInverse() scribing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getInverse()
    if(!is.null(inv)){
        message("Getting cached data....")
    
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
