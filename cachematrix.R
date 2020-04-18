## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
###############################################################################################################################################################################################################################
#DESCRIPTION:

# makeCacheMatrix() takes a number and make a squarematrix.Eg.If makeCacheMatrix(2), makes a 2X2 matrix and creates its inverse and saves it in the cache.
#cacheSolve fn returns the inverse of a matrix(x) which is given in makeCacheMatrix.

################################################################################################################################################################################################
#USAGE

# 1.Create a matrix using makeCacheMatrix([number]) and assign it to a unknown variable.Eg t<-makeCacheMatrix(3) - This gives a 3X3 matrix.
# 2.Pass the created variable in cacheSove([created variable]).Eg.cacheSolve(t) - this calculates and prints the inverse of the matrix.
# 3. Repeat the 2nd step to confirm the cache retrieval.
# 4. To check whether the program runs correctly, check with this cacheSolve(t)%*%t$get()
# 5.The above step prints the identity matrix.
#####################################################################################################################################################################################################################################

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  sq_x<-x*x
  x<-matrix(data=rnorm(sq_x),nrow=x,ncol=x)
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}
