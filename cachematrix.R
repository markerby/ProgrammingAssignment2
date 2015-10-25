## makeCacheMatrix and cacheSolve are used together to compute and cache the
## inverse of a matrix. First, call makeCacheMatrix() on invertible matrix x to
## create and return a list of set, get, setinverse and getinverse functions
## that act on the associated matrix x. Second, call cacheSolve() on the list 
## returned by makeCacheMatrix to a) return the cached inverse if it has already
## been computed and cached or b) if not previously calculated, to compute, 
## cache and return the inverse.
##
## Example: 
## > A <-makeCacheMatrix(matrix(c(2,0,-6,2,4,5,1,-1,-3),nrow=3,ncol=3))
## > cacheSolve(A)


## Creates a special matrix object that can cache its inverse. Assumes
## that the matrix argument x supplied to the function is invertible.
##
## Args:
##   x:  A matrix whose inverse is to be cached. It is assumed that this
##       matrix is invertible.
##
## Returns a list of functions to set and get the matrix and to set and get the 
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()){
        
        ## assign parent env m to NULL; this is the "cache"
        m <- NULL
        
        ## set function: sets parent env variable x to arg y
        ## clears the "cache" (the parent env variable m) by setting it to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get function: returns the matrix argument x 
        get <- function() {
                x    
        }
        
        ## setinverse function: caches the matrix inverse by setting the
        ## parent env variable m to the value matrixinverse
        setinverse <- function(matrixinverse){
                m <<- matrixinverse
        }
        
        ## getinverse function: returns the calculated or cached value of the
        ## matrix
        getinverse <- function(){
                m
        }      
        
        ## return a list of created set, get, setinverse, getinverse functions 
        ## used to perform work on associated matrix argument x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Computes and caches the inverse of a matrix. Assumes the matrix is invertible
## and has been created through makeCacheMatrix() function.
##
## Args:
##   x:  A matrix created by calling makeCacheMatrix and for whom the 
##       inverse is calculated or retrieved from a cache.
## 
## Returns the inverse of the matrix if it has not previously been calculated, 
## the cached inverse if it has previously been calculated.

cacheSolve <- function(x, ...) {

        ## call the getinverse() function associated with the cache matrix argument
        m <- x$getinverse()
        
        ## if m is not null, the inverse has been calculated and cached; return
        ## this cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## else m is null, the matrix inverse has not been calculated;
        ## get the matrix associated with the cache matrix argument x
        data <- x$get()
        
        ## calculate the matrix inverse
        m <- solve(data, ...)
        
        ## call the setinverse function associated with the cache matrix
        ## argument x to cache the matrix inverse
        x$setinverse(m)
        
        ## return the calculated matrix inverse m
        m
        
}

