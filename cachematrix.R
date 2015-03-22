## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse.
# 2015-03-22 : msydor - tested with a 3x3 with values from web example and random 
# value from uniform distribution
#
# test program (calculates timings) ommitted to keep focus on assignment goals
#
#
#  > mat5 <- matrix(runif(9,1,100),3,3)
#  > test(mat5)
#  calculating new value
#  Time difference of 0.0009999275 secs  ! this is a run without cache benefit
#  Time difference of 0 secs             ! this is a run with cach benefit
#
# test machine is Win7 professional (64-bit), 16GB RAM, 3 GHz quad-core
#
  

makeCacheMatrix <- function(x = matrix()) {
      
      # initialize the result
      #
      m <- NULL
      
      # update the matrix
      #
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      # current matrix
      #
      get <- function() x
      
      # update the inverse matrix result
      #
      set_Inverse <- function(private) m <<- private
      
      # return the inverse matrix result
      #
      get_Inverse <- function() m
      
      list(set = set,                  # set new matrix
           get = get,                  # get matrix
           set_Inverse = set_Inverse,  # calculate new inverse
           get_Inverse = get_Inverse)  # get cached value
      
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. The inverse will be calculated once.  On the next call, 
# if the matrix has not changed, then the cachesolve will retrieve the 
# last calculated inverse from the cache.

cacheSolve <- function(x, ...) {
      
      # Return a matrix that is the inverse of 'x'
      #
      Inverse <- x$get_Inverse()
      
      # check if this is a valid result
      #
      if( is.null (Inverse)){
            # calculate a new value
            #
            message("calculating new value")
            mat <- x$get()
            Inverse <- solve(mat)
            
            # update cache
            #
            x$set_Inverse(Inverse)
      }
      # return the cashed or new value, as appropriate
      return (Inverse)
}
