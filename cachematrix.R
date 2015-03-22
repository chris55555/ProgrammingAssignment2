
## makeCacheMatrix returns a list of two functions get() and getinverse(); 
# the function get() return the original matrix x;
# and another function getinverse() which calculate its inversematrix and 
# the result is stored in variable m

makeCacheMatrix<- function(x = numeric()) {
  m <- NULL # set m default value
  z<<-x
  get <- function() x      # get original matrix x
  
  getinverse <- function(){
    m<<-solve(x)
    m    #returm inversematrix m
  }
  
  list(get = get,getinverse = getinverse)  # return a list of two functions
}

   
  ## cacheSolve: This function take the arguments y returned by makeCacheMatrix
  # above. If the inversematrix has already been calculated (when m is not null), 
  # then the cachesolve should retrieve the inverse from the cache m. Or it will
  # calculate to give inverse matrix and return m.  
  

  cacheSolve <- function(y, ...) {
    m <- y$getinverse()
    if(!is.null(m)) {         # if m is not null, retrive inversematrix from m 
      message("getting cached data")
      return(m)  
    }
    data <- y$get()   # or, retrive original matrix and calculate its inversematrix and return m  
    m <- solve(data, ...)
    y$getinverse(m)
    m
  }
  