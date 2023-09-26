## Put comments here that give an overall description of what your
## functions do

## makeCacheMaatrix will execute a function with matrix x where in 
## z will be a list of NULL value
## set will set the value of the matrix
## get will retrieve the value of the matrix
## set_inverse will set the value of the inverse of the matrix
## get_inverse will retrieve the value of the inverse of the matrix
## and will return a list of the values of set, get, set_inverse, and get_inverse
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) z <<- solve
  get_inverse <- function() z
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cachemean will execute a function with matrix x 
## where m is matrix x indexed to get_inverse IF z is NOT null it will return the value of z
## else data is matrix x indexed to get
## and z will solve for data where matrix x is indexed to set_inverse of z
## and will return z

cachemean <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$set_inverse(z)
  z
}