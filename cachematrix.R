## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL 
  set <- function(y=matrix())   #sets values for the matrix 
  {
    x <<- y    #sets the value of global variable x to new matrix y
    m <<- NULL #sets the value of global variable m to NULL
  }
  get <- function() x  #gets the values of the matrix
  setinverse <- function() m <<- solve(x) #calculates the inverse of the matrix
  getinverse <- function() m #displays the inverted matrix
  list(set = set, get = get,  #creates a list consisting of 4 functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 m<<- x$getinverse() #get the inverse of the matrix x
  if(!is.null(m))
  {
    print("retrieving value from cache") # inverse already present in cache
    return(m)
  }
  
  data <- x$get()
  m <- solve(data) #calculate inverse of new matrix
  x$setinverse()  #set the inverse of new matrix in cache 
  m       ## Return a matrix that is the inverse of 'x'
}
