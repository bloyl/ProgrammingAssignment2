## This file contains two functions to enable caching of matrix inverse
##
## makeCacheMatrix : is a function that stores a matrix and its inverse
##                      All of the computation happens in this function.
## cacheSolve      : is simply a wrapper that calls getInverse() on the
##                      input parameter


## makeCacheMatrix stores a matrix and caches its inverse
## three functions exists
##  set/get     : these set or get the internal matrix
##  getInverse  : returns the chached matrix inverse computing it if needed
##                    or requested.
##                    solve is used for square matrices, MASS.ginv is used
##                    for nonsuqare inverses if possible.
makeCacheMatrix <- function(x = matrix())
{
  #store the inverse of x
  inverseX <- NULL
  
  #set/get for matrix
  set <- function(y)
  {
    x <<- y           #set x
    inverseX <<- NULL #reset the inverse
  }
  get <- function() x
  
  #return the inverse of x
  getInverse <- function(useCache = TRUE)
  {
    if (det(crossprod(x)) == 0)
    {
      stop("matrix is not invertable")
    }
    if (useCache == TRUE & !is.null(inverseX))
    {
      message('using Cached inverse')
    }
    else
    {
      message('recomputing matrix inverse')
      dims <- dim(x)
      
      #if x is square just invert
      if (dims[[1]] == dims[[2]])
      {
        message('   matrix is square using solve')
        inverseX <<- solve(x)
      }
      #try to use MASS
      else if( require(MASS,quietly=TRUE) )
      {
        message('   matrix is not square using ginv from MASS package')
        inverseX <<- ginv(x)
      }
      #use x_inv = (x' * x)^(-1) * x'
      else 
      {
        message('   matrix is not square using x_inv = (x\' * x)^(-1) * x')
        inverseX <<- solve( crossprod(x) ) %*% t(x)
      }
    }
    #return inverseX
    inverseX
  }

  #make the methods available
  list(set = set, get = get,
       getInverse = getInverse)
}


## Based on the implementation of the makeCacheMatrix this is simply a wrapper
## to x$getInverse() and could be done away with.
cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  x$getInverse()
}
