## Comments

## The idea behind the assignment is to cache potentially time-consuming computations; 
## in this case, the inverse of a matrix, that many times can be hard to get, 
## especially when the matrix is very large. We are assuming that all matrices evaluated 
## through these functions are invertable!
## The script consists of two functions 'makeCacheMatrix' and 'cacheSolve' that create 
## a special object that stores a matrix and cache's its inverse.

## Write a short comment describing this function

## 'makeCacheMatrix' creates a "special matrix object", which really is a list that stores 
## four functions that return the following results:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mat<-NULL
        set<-function(y){
                x<<-y
                mat<<-NULL
        }
        get<-function() x
        setinv<-function(solve) mat<<- solve
        getinv<-function() mat
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)

}


## Write a short comment describing this function

## The function takes the object from the list created previously with 'makeCacheMatrix' that
## stores the inverse of a matrix. If that result existed in the Cache, the function returns 
## a message 'getting cached data'; otherwise, the function calculates and returns the inverse
## of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat<-x$getinv()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        matrix<-x$get()
        mat<-solve(matrix, ...)
        x$setinv(mat)
        mat
}
