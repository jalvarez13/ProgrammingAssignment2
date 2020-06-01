
## This function creates four functions that allow for the 
## calculation and recovery of the inverse of a matrix

function(x = matrix()) {
  
  i <- matrix(NA,nrow(x), ncol(x))  ## initalize a matrix with NA
                                    ## values for storing the 
                                    ## inverse
  set <- function(y) {              ## fnction for changing the 
                                    ## value of the matrix
    x <<- y
    i <<- matrix(NA,nrow(x), ncol(x)) ## resets the inverse
  }
  get <- function() x
  setinv <- function(inv) i <<- inv ## called from cacheSolve to 
                                    ## set the inverse
  getinv <- function() i            ## called from cacheSolve to
                                    ## the stored value of the
                                    ## inverse
  list(set = set, get = get,        ## Set names for allowing the 
                                    ## functions to be called
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.na(i[1,1])) {              ## checks if the stored value
                                    ## of the inverse has a NA in
                                    ## the first element. That would
                                    ## mean there is no inverse
    message("getting cached data")
    return(i)
  }
  data <- x$get()                   ## obtains the matrix
  i <- solve(data, ...)             ## calculates the inverse
  x$setinv(i)                       ## caches the inverse
  i
}
