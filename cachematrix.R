## Put comments here that give an overall description of what your
## functions do: These two functions will give us the inverse of an invertible
## matrix. Moreover, they will save time as the functions will compute the inverse
## only in case it has not been computed yet. After computation the inverse
## is saved into the memory and is ready for subsequent usage of it without the 
## need to be computed again. This saves memory of the computer.

## Write a short comment describing this function: makecachematrix takes 
## matrix as an input and creates a list of four functions. These are 
## subsequently used for setting and getting of the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  m=NULL
  
  ##function setting the matrix
  set=function(y){
    x <<- y
    m <<- NULL
  }
  
  ##function getting the matrix
  get=function() x
  
  ##function setting the inverse
  set_inverse <- function(inverse) m <<- inverse
  
  ##function getting the inverse
  get_inverse=function() m
  
  ##final list of functions
  list(set=set,get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function: cachesolve takes an output from 
## makeCachematrix as an input. Using this list this function first searches
## whether inverse has already cumputed and if yes it returns the value of the
##inverse. If nothing has been computed it calculates the inverse and stores it
##into the list.

cachesolve <- function(A, ...) {
  
  #searching for cached data
  m <- A$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #calculating inverse if cached data unavailable
  data <- A$get()
  m <- solve(data, ...)
  A$set_inverse(m)
  m
}