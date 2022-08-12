## Put comments here that give an overall description of what your
## functions do
##there are two function makeCacheMatrix and makeCacheMatrix
##makeCacheMatrix consist of set,get, setinv, getinv
## library(MASS) used to calculate inverse for non squared as well as square matrices
## Write a short comment describing this function
 library(MASS)
 makeCacheMatrix <- function(x = matrix())  {
     inv<-NULL      # initializing inverse as null
    set <-function(y){
               x<<-y
              inv<<-NULL
       }
  get<-function()x       #function to get matrix
     setinv<-function(inverse)inv<<-inverse
    getinv<-function(){
         inver<-ginv(x)
            inver%%x    #function to obtain inverse of the matrix
        
        }
    list(set = set, get = get,
                   setinv = setinv, 
                   getinv = getinv)
    
     }

   ## write a short comment describing this function
   ## this is used to get the cachedata
   cachesolve<- function(x,...)  ##gets cachedata
     {
         inv<-x$getinv()
         if(!is.null(inv)){       ##checking whether inverse is Null
             message("getting cached data!")
             return(inv)     ##returns inverse value
             
               
             }
      data<-x$get()
         inv<-solve(data,...)     #calculates inverse value
         x$setinv(inv)
         inv            # return a matrix that is the inverse of "x"
       }
   f<-makeCacheMatrix(matrix(1:8,2,4))
   f$get()
f$getinv()   
cachesolve(f)
