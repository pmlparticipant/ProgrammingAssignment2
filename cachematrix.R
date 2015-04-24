# The following two functions allow for creating and storing a matrix (makeCacheMatrix) and for calculating the inverse of such matrix (cacheSolve).

	# The following function creates an object with four methods: "set", "get", "setinv" and "getinv". These methods are required in the cacheSolve function described below.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y # using the <<- operator we have access to x which is beyond the current environment
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv # this setter saves the value inv to the outside variable i (associated with the object created by makeCacheMatrix) 
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The following function calculates OR accesses the inverse of matrix x.
cacheSolve <- function(x,...) {
  i <- x$getinv()
  if(!is.null(i)) { # if the inverse has already been calculated, return the value of i
    message("getting cached data")
    return(i)
  }
  # otherwise, calculate the inverse for the first time
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}

