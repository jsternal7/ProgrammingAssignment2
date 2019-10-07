## cachematrix.R
## Author: Jack Sternal
## Date: 07 Oct 2019

## makeCacheMatrix will take in a matrix (x) and create 4 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){ #sets the matrix (x in the parent environment) to the input matrix (y) and sets the matrix inverse (i in the parent environment) to NULL
   x <<- y
   i <<- NULL 
  }
  get <- function() x #returns the original input matrix
  setinverse <- function(inv) i <<- inv #sets the inverse of the matrix
  getinverse <- function() i #returns the inverse of the matrix
  list(set=set, get=get, setinverse = setinverse, getinverse=getinverse) #allows for use of $ extract operator to invoke functions by name
}


## cacheSolve will take in a matrix (x) and check to see if the inverse is cached. 
## If cached, the inverted matrix will be returned, along with a message informing the user that cached data 
## is being retrieved. Otherwise, the inverse of the input matrix (x) will be calculated and returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("retrieving cached matrix") #conditional to check if inverted matrix is cached and return if so.
    return(inv)
  }
  data <- x$get() # pull matrix from makeCacheMatrix function
  inv <- solve(data,...) # use solve() function to invert matrix
  x$setinverse(inv) # set inverse to previously called solve function
  inv #return inverted matrix
}
