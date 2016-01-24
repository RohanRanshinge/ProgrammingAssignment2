## Defining the function such that it can cache a matrix
## 'setMatrixSolve' is a function that stores the inverse of the matrix 
## 'getMatrixSolve' is a function that calls the inverted matrix
## 'get' 
makeCacheMatrix <- function(x = matrix()) {
 i<-NULL

 set<-function(y){
  x<<-y
  i<<-NULL
  }
 
 get<-function() x
 
 setMatrixSolve<-function(inv) i<<-inv
 
 getMatrixSolve<-function() i
 
 list(set=set, get=get, setMatrixSolve=setMatrixSolve, getMatrixSolve=getMatrixSolve)
}


## This function fetches the matrix if it is in cache or else it computes the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getMatrixSolve()
        ## If in cache memory the inverted matrix is returned 
        if(!is.null(inverse)){
        message("Getting cached matrix")
        return(inverse)
        }

        data<-x$get()

        inverse<-solve(data,...)
        ## The inverted matrix is obtained
        x$setMatrixSolve(inverse)

        inverse
        }

