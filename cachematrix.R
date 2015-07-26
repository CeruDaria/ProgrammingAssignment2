## The following function is used to cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
          
          ## The inverse object holds the cached inverse of a given matrix
          ## The object is set to NULL initially
          inverse <- NULL
          
          ## The setmatrix function stores a matrix as specified by its agrument
          setmatrix <- function(y) {
                    x <<- y
                    ## Because a new matrix is set, the cache is set
                    ## back to NULL to make room for a new value.
                    inverse <<- NULL
          }
          ## Return the stored matrix
          getmatrix <- function() x
          
          ## Cache a value as given by the argument
          setinverse <- function(solve) inverse <<- solve
          
          ## Return the cached value
          getinverse <- function() inverse
          
          ## Specify 4 lists for an object if the function is assigned to an object
          list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}
## The following function calculates the cached matrix by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
          ## Return the cached value
          inverse <- x$getinverse()
          
          ## If there is cached data, return it
          if(!is.null(inverse)){
                    message("getting cached data")
                    return(inverse)
          }
          ## Else calculate the inverse of x
          ## First, store the matrix in an object
          matrix <- x$getmatrix()
          
          ## Calculate the inverse
          inverse <- solve(matrix, ...)
          
          ## Cache the inverse
          x$setinverse(inverse)
          
          ## Return the inverse
          inverse
}
