
# This function returns a list contaning functions to 
# 'set' to set the value of a matrix
# 'get' to get the value of a matrix
# 'setmatrix' to get the cached value (inverse of the matrix)
# 'getmatrix' to get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {


  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cache<-NULL

  # store a matrix
  setMatrix<-function(newValue){
  x<<-newValue
  cache<<-NULL
}

# returns the stored matrix
getMatrix<-function() x

# cache the given argumen
cacheInverse<-function(solve) cache<<- solve

# get the cached value
getInverse<-function() cache

# return a list. Each named element of the list is a function
list(setMatrix=setMatrix, getMatrix=getMatrix,
   cacheInverse=cacheInverse,
   getInverse=getInverse)

}


# This function is used to find the inverse of a matrix and cache it using a free floating variable. 
# However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation.

cacheSolve <- function(x=matrix(), ...) {

    # get the cached value
    inverse<-x$getInverse()

    # if a cached value exists return it
    if(!is.null(inverse)){
      message("getting cached data")
      return(inverse)
    }

    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    matrix<-x$getMatrix()
    inverse<-solve(matrix, ...)
    x$cacheInverse(inverse)

    # return the inverse
    inverse
}
