## The first function basically "creates" a "special" matrix, whose inverse we calculate 
# the objects (related to the matrix itself and its inverse) created within it are "superassigned" 
# so that they can be found by the second function 
# the second function 'aims' to return the inverse of the special matrix assigned
# but does this in a way to save on computation effort: 
# it checks the objects from the first function - 
# if the one relating to the inverse is already assigned then it will just display that 
# if not, then it will calculate the inverse anew, and show it - 
# and also assign that inverse to relevant object in the first function 
# so that it won't have to recalculate later when asked to again. yay.  

## 1. "creation" of the matrix, objects relating to its inverse 

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL # our inverse, set to null each time function is called
      set <- function(y) {
            x <<- y # superassign x like this so that we can find x and [re-] set it outside of this function's environment
            s <<- NULL # superassign s to null so can be reached outside of this function 
      }
      get <- function() x # return value of original matrix 
      setinverse <- function(solve) s <<- solve # called by cacheSolve during first cachesolve, stores value by superassignment
      getinverse <- function() s # returns value to cacheSolve on subsequent calls 
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse) # internal functions, so that the calling function knows how to access these methods 
}

## 2. return the inverse of the matrix above, save on computation if can

cacheSolve <- function(x, ...) { # x= a matrix made by makeCacheMatrix
      s <- x$getinverse() # access x, get its inverse from previous function
      if(!is.null(s)) { # if it's not null, then
            message("getting cached data") # show this message 
            return(s) # and return the value 
      }
      data <- x$get() # if above not true, then we're here; assign value to x 
      s <- solve(data) # solve this data i.e. get inverse 
      x$setinverse(s) # and then assign this value to setinverse in above function for if/when called later 
      s
        ## Return a matrix that is the inverse of 'x'
      
}
