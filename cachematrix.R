## The program uses functions makeCacheMatrix and cacheSolve to speed-up
## the calculation of the inverse by caching it's value.

## makeCacheMatrix returns a special "matrix" in a form of a list of functions which
## 1. Get the value of the matrix
## 2. Set the value of the matrix
## 3. Get the value of the inverse matrix
## 4. Set the value of the inverse matrix


makeCacheMatrix <- function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<- y
    inv<<- NULL
  }
  get <- function() x
  setinv <- function(Inverse) inv<<- Inverse
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve returns the inverse of the matrix x. If the matrix inverse is pre-calculated
## (x remains unchanged) then it returns the inverse value from the cache.

cacheSolve <- function(x,...){
  Inverse = x$getinv()
  if(!is.null(Inverse)){
       message("returning cached value")
       return(Inverse)
  }
  data <- x$get()
  Inverse<- solve(data)
  x$setinv(Inverse)
  Inverse # return a matrix inverse of x
}

