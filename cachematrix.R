## Put comments here that give an overall description of what your
## functions do

## To make this code work, you will first need to call
## "cacheSolve(makeCacheMatrix())", finally,
## to get it to work you call makeCacheMatrix(x), in which "x" is a 
## square matrix also,
## you are gonna need to run "m <- matrix(0,x,1)" , substituting x for a number
## equal to the length of the matrices you wish to find and store the solve,
## example: m <- matrix(0,4,1) # This will work for a matrix with 2 rows and
## 2 columns. IMPORTANT: All matrices will be stored in "m" as columns
## and must have the same length(you won't be able to store 
## 3x3 and 2x2 matrices at the same time).
##

## Write a short comment describing this function
m <- matrix(0,4,1)

## The function that calls the get function
makeCacheMatrix <- function(x = matrix()) {
  
  ## Searches the input matrix in m, if it can't find it, then  the set function
  ## will be called
  get <- function(x){
    nc <- ncol(m)
    dim(x) <- c(length(x), 1)
    
    for (i in (1:nc)) {
      
      if (nc %% 2 == 1) {
        
        if((all(x == m[,i])) && (i==(nc-1))){
          print("getting matrix")
          print(m)
          g <- m[,i]
          print(g)
        } else {
          set(x)
        }
      } else {
        if((all(x == m[,i]))){
          print("getting matrix")
          print(m)
          g <- m[,i+1] ##i+1
          print(g)
          break
        } else {
          if(i==(nc-1)){
            set(x)
            break 
          }
          
        }
      }
    }
  }
  
  ## Stores a new matrix in m and calls the getsolve function
  set <<- function(x) {
    
    if(all(m == 0)){
      d <- dim(x)
      
      if(d[2] == 1) {
        dim(x) <- c(length(x), 1)
        m <<- x
        getsolve(x)
      } else {
        print("Invalid matrix")
      }
    } else {
      dim(x) <- c(length(x), 1)
      m <<- cbind(m,x)
      print(m)
      getsolve(x)
      
    }
    
    print(m)
    
    if((ncol(m) %% 2) == 1 ){
      getsolve(x)
      print("x in set() de makeCacheMatrix() ")
      print(x)
    }
    
  }
  
  get(x)
}

cacheSolve <- function(x){
  
  ## Reads m and looks for the solve of the matrix, or calls the setsolve function
  getsolve <<- function(x) {
    dim(x) <- c(length(x), 1)
    nc <- ncol(m)
    
    
    for (i in (1:nc)) { 
      
      if (i %% 2 == 0) {
        
        if ((all(x == m[,i]) == T) && (i == nc)) {
          g <- m[,i]
        } 
        
      } else {
        if(i==nc){
          # m <<- cbind(m,x)
          d <- sqrt(length(x))
          dim(x) <- c(d,d)
          setsolve(x)
          print(m)
          break
        }
      }
      
    }
  } 
  
  ## Uses the solve function on a matrix and stores it in m
  
  setsolve <- function(x){
    d <- sqrt(length(x))
    dim(x) <- c(d, d)
    s <- solve(x)
    dim(s) <- c(length(s), 1)
    print(m)
    print(s)
    m <<- cbind(m,s)
  }  
}