## In this assignment we endeavour to taggle around the computation cost of Matrix Inversion of very large size matrixes. 
## The aim is to cache the inverse matrix than compute it repeatedly, that is to save it and store it somewhere and avoid having to recompute
## the same inverse matrix and generate the same results.
## The following two functions were created as a potential solution to cache the inverse of a matrix.

## makeCacheMatrix:
## First we create a special matrix that will help us by using the makeCacheMatrix function.  
##The input is a simple matrix, as an example: 
## x <- matrix(rnorm(4), nrow=2, ncol=2)
## m <- makeCacheMatrix(x)


## Following the same format as the assignment example
## Creating a makeCacheMatrix object will consist of
## four functions encapsulated in a list
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Initially the inverse is set to NULL, as it will take differnt value later on
  set <- function(y) { #The set function simply sets the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x #Get function, gets the matrix
  setinv <- function(inverse) inv <<- inverse #Here the matrix is set to inverse
  getinv <- function() inv #we get the inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv) #save it into a list
}

## The following function will return the inverse of the matrix. 
##The function checks if the inverse already exist, if it does it will display the pre-computed result and thus skip the re-computation. 


cacheSolve <- function(x, ...) {
  inv <- x$getinv() # Initially the funciton checks if the inverse matrix has been computed
  if(!is.null(inv)) { #If it has
    message("getting cached data.") #it simply returns the result
    return(inv)
  }
  data <- x$get() #If not, it will get the matrix
  inv <- solve(data, ...) #compute the inverse 
  x$setinv(inv) #and cache it in the object
  inv
}

##Checking code:
 x <- matrix(rnorm(4), nrow=2, ncol=2)
 m <- makeCacheMatrix(x)
 m$get()
 cacheSolve(m)
 
