## Second practice o
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      vout <- NULL #solved matriz
      vin  <- NULL #in-matriz
      
      # $set: if new matrix is diferent, clear inverse result
      set <- function(vnew) {
            if (!identical(vnew, vin)) {
                  message("new matrix set & clear cache")
                  vin <<- vnew
                  vout <<- NULL #clear cache
            }
      }
      
      # $get: return matrix in-matrix
      get <- function()
            vin
      
      # $setinv: cache result
      setinv <- function(xout) {
            vout <<- xout
      }
      
      # $getinv: return result cache
      getinv <- function()
            vout
      
      #  first call create objet with $set (x)
      if (!is.null(x))
            set(x)
      
      # return:
      list(
            set = set,
            get = get,
            setinv = setinv,
            getinv = getinv
      )
}


## Return inverse matrix and only calc firts time for single parameter
## WARNING: only cache for not adds paramenters in solve function

cacheSolve <- function(x, ...) {
      # get cache data
      m <- x$getinv()
      
      # calc matrix inverse if not calc previusly or if pass more parameters  
      if (is.null(m) || nargs() > 1) {
            message("calc inverse")
            data <- x$get()
            m    <- solve(data, ...)
            
            #store result if only pass one parameter
            if(nargs()==1) x$setinv(m) 
      } else message("getting cached data")
   
      
      # return:
      m
      
}
