## The purpose of this function is to cache potentially time-consuming
## matrix inversion computation by using the scoping rules in R and the
## operator "<<-" that assing a value to the parent/global existing
## variable rather than the variable in the local environment.


## makeCacheMarix creates a special "matrix", 
## which is a list containing a function to
##    1. set(): set the value of the matrix
##    2. get(): get the value of the matrix
##    3. setinv(): set the value of the matrix inverse
##    4. getinv(): get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
     invMat <- NULL
     set <- function(y) {
          x <<- y
          invMat <<- NULL
     }
     get <- function() x
     setinv <- function(inv) invMat <<- inv
     getinv <- function() invMat
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## cacheSolve calculates the inverse of the special "matrix" 
## created with the above function "makeCacheMatrix". 
## However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse matrix from the cache
## and skips the computation. Otherwise, it calculates the inverse 
## of thematrix and sets the value of the inverse in the cache 
## via the setinv function.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     invMat <- x$getinv()
     if(!is.null(invMat)) {
          ## getting cached inverse matrix
          return(invMat)
     }
     ## othewise calculate the invrse using solve()
     data <- x$get()
     invMat <- solve(data, ...)
     x$setinv(invMat)
     invMat
}
