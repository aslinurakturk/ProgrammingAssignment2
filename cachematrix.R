## These functions are used in combination to construct a "special" matrix, 
## calculate its inverse unless it has already been calculated,
## and cache the inverse matrix so that there will be no need to go through the
## process of calculating it again.

## makeCacheMatrix constucts a "special matrix" which is actually contained in a 
## list with attributes set, get, setinverse, and getinverse all of which are 
## functions.

makeCacheMatrix <- function(x = matrix()) {
       inv<-NULL
       set<-function(y){
               x<<-y       ##"<<-" allows searching for the value to be assigned 
               inv<<-NULL  ## in environments other than the current environment. 
       }                   
       get<-function() x
       setinverse<-function(inverse) inv<<-inverse
       getinverse<- function() inv
       list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) 

}


## cacheSolve takes as one its arguments the "special matrix" obtained from 
## makeCacheMatrix. The function retrieves the cached inverse matrix and prints 
## a message if x$getinverse does not return NULL. Otherwise it calculates and 
## returns the inverse of the matrix obtained from makeCacheMatrix using the 
## solve function.

cacheSolve <- function(x, ...) {
       inv<-x$getinverse() 
       if(!is.null(inv)){
               message("getting cached data")
               return(inv)
       }
       data<-x$get()
       inv<- solve(data,...)
       x$setinverse(inv)
       inv
}
