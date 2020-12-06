makeCacheMatrix <- function(matrix = matirx()) {
  matrixInverse <- NULL
  set <- function(y) {
    matrix <<- y
    matrixInverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function(inverse) matrixInverse <<- inverse
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(specialMatrix, ...) {
  matrixInverse <- specialMatrix$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- specialMatrix$get()
  matrixInverse <- solve(data, ...)
  specialMatrix$setInverse(matrixInverse)
  matrixInverse
}
