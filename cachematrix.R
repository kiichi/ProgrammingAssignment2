###################################################
# Week 3 Assignment
# Kiichi Takeuchi
# 4/25/2014
# Calculate Inverse Matrix with caching mechanism
###################################################

#--------------------------------------------------------
# This function creates a matrix with caching mechanism
# Parameters:
#   - matrix
# Return:
#   an object/function that contains cache
#   use setSolve() to set the cache matrix
#   then getSolve() returns the cached version of result
#   for the next time
#--------------------------------------------------------
makeCacheMatrix <- function(mData = matrix()) {
  mFunc <- NULL
  
  # Get the cached data
  get <- function(){
    mData
  }
  
  # Set the cache value & reset the function
  set <- function(inVal){
    #Initializing those member variables
    #in outside scope
    mData <<- inVal
    mFunc <<- NULL
  }
  
  # Get the reference to the function 
  getSolve <- function() {
    mFunc
  }
  
  # Set the function
  setSolve <- function(inFunc) {
    mFunc <<-inFunc
  } 
  
  #expose member variables as the list
  list(
    get=get,
    set=set,
    getSolve=getSolve,
    setSolve=setSolve
  )  
}

#-------------------------------------------------------------
# This is a wrapper function of solve() 
# which takes the chached matrix in the argument
# This version of solve function will avoid
# re-calculating the inverse of the input matrix
# 
# Parameter : 
#   object that is created by makeCacheMatrix() function
# Return:
#   result of solve() function
#   it will not re-compute if the result is already calculated.
#-------------------------------------------------------------
cacheSolve <- function(cacheMatrix, ...) {
  
  result <- cacheMatrix$getSolve()
  if (!is.null(result)){
    message("Getting cahced data")
    return(result)
  }
  
  result <- solve(cacheMatrix$get(),...)
  cacheMatrix$setSolve(result)
  result  
}


#------------------------------------------
# Test Code : A * A-1 = I
#------------------------------------------
# mat_cache<-makeCacheMatrix(matrix(c(4,3,3,2),2,2) )
# print("A --------------------")
# mat<-mat_cache$get()
# print(mat)
# print("A-1 --------------------")
# mat_inv <- cacheSolve(mat_cache)
# print(mat_inv)
# print("I --------------------")
# print(mat%*%mat_inv)

#------
# Should get Identity matrix as the result
#       [,1] [,2]
# [1,]    1    0
# [2,]    0    1

#------
#try this in console, and you should get the message about cache
#> cacheSolve(mat_cache)


