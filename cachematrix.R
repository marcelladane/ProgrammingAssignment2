#  R function is able to cache potentially time-consuming computations
## This function will be caching the Inverse of a Matrix 

# The first step is to create a matrix object that can cache its inverse
## Functions to be used	
### 'inv'  (Invert numeric or complex matrix, works with solve(c))
### 'set' (set function just allows to set the values of an object => returned by makeVector without having to call makeVector again)

## <<- assign a value to an object in an environment that is different from the current environment 

#This is done in 4 steps
## 1- set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  	set <- function(y){
    		x <<- y
    		inv <<- NULL
}
## 2 - get the value of the matrix
 get <- function() x

## 3 - set the value of the invMatrix
 setInv <- function(solveMatrix) inv <<- solveMatrix

## 4 - get the value of the invMatrix and make it as a list
 getInv <- function() inv
 list(set = set, get = get, setInv = setInv, getInv = getInv)
}


#  Calculates the mean of the inverse Matrix created with the above function
## takes as input the output of the other function

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

inv <- x$getInv()

## if the value was not calculated yet, you get the message and the calculation
if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  return(inv)      
}

