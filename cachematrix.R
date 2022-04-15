setwd('C:/Users/decla/OneDrive/Documents/GitHub/ProgrammingAssignment2')

## Our aim is to write 2 functions makeCacheMatrix and cacheSolve

##makeCacheMatrix is a function which create a matrix object that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
          x <<- y
          inv <<- NULL
          }
    get <- function()x                      #function to get the matrix x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv            
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##cacheSolve is a function which computes the inverse of the matrix returned by the makeCacheMAtrix above 

cacheSolve <- function(x, ...)    #Returns a matrix that is inverse of 'x'
  {
  inv <- x$getinv()
  if(!is.null(inv)) {                                 #checking if null
                  message("getting cached result!")
                  return(inv)                         #returns inverse value
                  }
  data <- x$get()
  inv <- solve(data, ...)                             #calculates inverse value
  x$setinv(inv)
  inv                                         
}
