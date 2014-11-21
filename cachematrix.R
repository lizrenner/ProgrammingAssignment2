## These functions take a matrix and calculate and
## cache the inverse of the matrix.

## This function, makeCacheMatrix, takes a matrix object x, calculates 
## its inverse using the solve() function, and caches the result of
## solve(). It returns a list with 4 objects ('set'[the matrix with inve 
## set as NULL], 'get'[the matrix but inve not set as NULL]
## 'setinverse' [calculates/stores the inverse of the matrix in inve],  
## and 'getinverse' [returns inve if already calculated])


makeCacheMatrix <- function(x = matrix()) {      
  inve <- NULL           
  
  # initializes inve, which will eventually get the value of the inverse
  # of the matrix; resets it to NULL every time fxn is called.
  
  set <- function(z) {
    x <<- z
    inve <<- NULL
  }
  
  # set takes input matrix z, saves it as x, resets the inverse to NULL
  
  get <- function() x      # get returns the value of the original matrix
  
  setinverse <- function(solve) inve <<- solve
  # setinverse is called by cacheSolve(). it calculates the inverse of
  # the matrix and stores its value by superassignment
  getinverse <- function() inve
  # getinverse returns
  # the value of setinverse if it has been cached
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  # a list of the internal functions of this fxn
  # returned whenever makeCacheMatrix() is called
}


## This function, cacheSolve, returns the inverse of the matrix X 
## by returning getinverse from makeCacheMatrix if the inverse
## has already been calculated (and matrix has not changed), or by
## calling setinverse if the inverse has not been calculated.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inve <- x$getinverse()  # 'inve' is set to the value of getinverse
                          # from makeCacheMatrix above
  if(!is.null(inve)) {    # if 'inve' is not null (has been set by
                          # setinverse and stored in getinverse)
    message("getting cached data")  # the fxn returns this message
    return(inve)          # and the value of 'inve'
  }
  data <- x$get()         # otherwise, the fxn sets 'data' as the original 
                          # matrix
  inve <- solve(data, ...)  # returns the inverse of the matrix
  x$setinverse(inve)      # stores the inverse matrix
  inve                    # and returns the inverse matrix
}





