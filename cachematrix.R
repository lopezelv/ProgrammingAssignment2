## this function creates a special "matrix" object than can cache its inverse
## we intitialize the mean to NULL since we want to calculate the mean in the 
## in the cachemean 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## this function computes the inverse of the special matrix which is returned by makeCacheMatrix
## we are setting the mea of the matrix x
cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("Returning Cached Data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  (m)
}
