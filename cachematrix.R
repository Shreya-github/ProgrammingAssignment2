##This function creates a special "matrix" object that can cache its inverse
##This function comprises of set the value of the vector, get the value of vector
## set the inverse and get the inverse.


makeCacheMatrix <- function(x = matrix()) {
 j <- NULL
 set <- function(y){
 x <<- y
 j <<- NULL
 }
 get <- function()x
 setInverse <- function(inverse)j <<- inverse
 getinverse <- function()j
 list(set = set, get = get,
 setInverse = setInverse,
 getInverse = getInverse)
}


## The following function calculates the inverse of the special "vector" 
##created with the above function. However, it first checks to see if the inverse
##has already been calculated. If so, it gets the inverse from the cache 
##and skips the computation. Otherwise, it calculates the inverse 
##of the data and sets the value of the inverse in the cache via the setInverse
##function.

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

