## He we have two functions that compute inverse of a matrix.
## An optimisation has been done for caching inverse matrix, we just compute it onetime for better performance.


#return an object for matrix x, that embed it's inverse matrix for caching purpose.
#the object has some helper function to set/get matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) 
{
  #internal inverse matrix 
  m <- NULL
  
  #setter function to change internal matrix and to invalidate cached inv matrix
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }

  #getter function to get internal matrix
  get <- function() x
  
  #function that set internal inverse matrix
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  #retunred list of function
  list(set = set, get = get, setinv = setinv,  getinv = getinv)
}


## Return a matrix that is the inverse of 'x'.
## It  first check if value has already been stored in cache otherwise compute it and store it in cache
cacheSolve <- function(x, ...) 
{  
  #store invers matrix of x in m
  m <- x$getinv()

  #return inverse matrix if we found it in cache
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  #here we have a cache miss
  
  #get our matrix
  data <- x$get()
  
  #get inverse of our matrix
  m <- solve(data, ...)
  
  #store inverse in cache
  x$setinv(m)
  
  #our matrix
  m  
}

## helper function for testing purpose only
## it return a random 2d rotation matrix
## rotation matrix are orthogonal =>  Q−1 = QT
## > a<-getrandomrotmatrix()
## > b<-makeCacheMatrix(a)
## > cacheSolve(b) %*% a
## > sould return an identity matrix
getrandomrotmatrix <- function()
{
  theta <- runif(1, 0, 2.0*pi)
  matrix(c(cos(theta) , -sin(theta) , sin(theta) , cos(theta)), nrow=2, ncol=2)
}
