
## This function creates four functions that allow for the 
## calculation and recovery of the inverse of a matrix



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
