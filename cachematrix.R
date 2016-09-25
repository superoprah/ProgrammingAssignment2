#makeCacheMatrix function basically is the skeleton for the special cache matrix object.
#Coerces input to a matrix and lays out foundation for inversed matrix to be stored in the object
#Keeps the original matrix in tact, and also holds the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inv) m <<- inv
  getmatrix <- function() m
  list (set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Checks if matrix has already been inverted, if it has then it just returns the calculation from before.
#Otherwise it goes ahead and uses solve to invert the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m
}

#example solution for matrix(1:4,2,2)
# > g<-makeCacheMatrix(matrix(1:4,2,2))
# > cacheSolve(g)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(g)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

