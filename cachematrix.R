## R Programming Assignment 2: Lexical Scoping-
## 40 character SHA-1 hash
## https://github.com/amitshinde0511/ProgrammingAssignment2

## The makeCacheMatrix function creates a special matrix",


a <- makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(inverse) m<<- inverse
  getmatrix<-function() m
  list(set=set, 
       get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates inverse of the special "matrix"

b <- cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

#### Testing matrix below 
 m = matrix(c(22,44,33,11), nrow=2, ncol=2)
 m
 solve(m)

