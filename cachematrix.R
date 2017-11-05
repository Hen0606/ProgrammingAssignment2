## Below functions provide a way to store inverse of a matrix to a list that consist of itself,
## its inverse and something else so that you will not be calculating the inverse of a matrix again
## after you have done so.

## makeCacheMatrix create a "shadow" that would tag a matrix's inverse once you calculated the inverse use cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
       set <- function(y){
             x <<- y
             i <<- NULL
             }
       get <- function() x
       setinverse <- function(inverse) i <<- inverse
       getinverse <- function() i
       list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse)
}


## if cacheSolve has already calculated the inverse of a matrix earlier, it would simply check its "shadow" created by 
## makeCacheMatrix and get the inverse instead of calculate again.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)){
                message("getting cached data")
                return(i)
                }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
        ## Return a matrix that is the inverse of 'x'
      }
