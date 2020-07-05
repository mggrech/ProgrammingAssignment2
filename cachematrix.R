## This file contains 2 functions: makeCacheMatrix() and cacheSolve()
##
## makeCacheMatrix() creates and R object that stores
## a matrix and its inverse. The object created by makeCacheMatrix() is used
## as an argument to cacheSolve().
##
## cacheSolve() retrieves the inverse from the cached value stored in the
## environment of the makeCacheMatrix() object if it exists (and the matrix has 
## not changed) or computes it if it does not exist.


## BRIEF DESCRIPTION OF makeCacheMatrix()
## The function makeCacheMatrix() creates and returns an object of type 
## makeCacheMatrix() to be used by cachSolve(). 
## It first initializes 2 objects, x (initialized as function argument) and im. 
## It then defines 4 functions: 
## (1) the set() function assigns the input argument y to the x object in 
## the parent environment and assigns NULL to the im object in the parent 
## environment, clearing any value of im that has been cached by a prior 
## execution of makeCacheMatrix().
## (2) the get() function retrieves x from the parent environment of 
## makeCacheMatrix().
## (3) the setinv() function assigns the input argument inv to the value of
## im in the parent environment.
## (4) the getinv() retrieves the value of im from its parent environment.
## In the end, these 4 functions are assigned as elements within a list() which
## is returned to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y){
            x <<- y
            im <<- NULL
    }
    get <- function() x
    setinv <- function(inv) im <<- inv
    getinv <- function() im
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## BRIEF DESCRIPTION OF cacheSolve():
## cacheSolve() takes an argument of type makeCacheMatrix(). 
## It first tries to retrieve the matrix inverse from the
## object x passed as its argument by calling the getinv() function and 
## assigning it to im. If im is not NULL, then the inverse exists and is 
## retrieved from cache and returned to the parent environment.
## If im is NULL (inverse does not exist), it uses get() to retrieve the matrix  
## from the input object x and solve() to compute its inverse.  
## The inverse is then assigned to the input object using setinv().
## Finally cachSolve() prints the value of the inverse which returns it to 
## the parent environment. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    im <- x$getinv()
    if(!is.null(im)){
            message("Getting cached inverse")
            return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinv(im)
    im
}