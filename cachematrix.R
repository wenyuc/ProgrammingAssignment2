

## The first function, makeCacheMatrix create a special vector
## which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    
    x <<- y
    
    inv <<- NULL
    
  }
  
  get <- function() x
  
  setinv <- function(solve) inv <<- solve
  
  getinv <- function() m
  
  list(set = set, get = get,
       
       setinv = setinv, getinv = getinv)
  
}



## The following function calculates the inv of the special
## "vector" created with the above function.
## However, it first check to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and
## skip the computation. Otherwise, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <-x$getinv()
  
  if(!is.null(inv)){
    
    message("getting cached data")
    
    return(inv)
    
  }
  
  data<-x$get()
  
  inv<- solve(data, ...)
  
  x$setinv(inv)
  
  inv
  
}
