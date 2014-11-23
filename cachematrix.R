## Function to convert matrix in list that stores original matrix, inverse value,
## and initial matrix and initial inverse as 0 value

makeCacheMatrix <- function(x = numeric()) { ## x is input vector (=matrix here)
  inv <- NULL ## value inv matrix, reset when makeCacheMatrix is called
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() x ## return value of x=original matrix
  setsolve <- function(solve.res) inv <<- solve.res ## called by cacheSolve at 1st access
  ## superassigns inv value in cache
  getsolve <- function() inv ## called by cacheSolve subseq. access, retrieve inv from cache
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) ## called for each new matrix, generates list of functions
}

## Function that checks whether the inverse of an input matrix is stored, if so
## print result on screen, otherwise calculates the inverse, stores in cache and
## print on screen. Input is result from feeding first a matrix to makeCacheMatrix 

cacheSolve <- function(x, ...) { ## return the inverse of a matrix x
  inv <- x$getsolve() ## obtain inv value as stored using makeCacheMatrix
  if(!is.null(inv)) { ## check if there is a value for inv (not NULL) if not go to data line
    message("getting cached data")
    return(inv) ## print stored inverse matrix and end cacheSolve
  }
  data <- x$get() ## get value of original matrix
  inv <- solve(data, ...) ## calculate inverse for new matrix and store in inv
  x$setsolve(inv) ## set inv in cache
  inv ## print calculated inv value
}
