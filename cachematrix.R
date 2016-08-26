## The following function calculates and caches the Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## The makeCacheMatrix function, creates a special "matrix" with caching. The funciotn has 4 methods: 
## set and get: to assign and retrieve the matrix to be calculated.
## setinverse and setinverse: to assign and retrieve the inverse of the matrix stroed in this object.


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix in the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix and sets the inverse of the matrix in the cache
## via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


## Sample RUN:
## #inialize a matrix
# > x = matrix(1:4, 2, 2)
# 
# > m = makeCacheMatrix(x)
# 
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# 
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
