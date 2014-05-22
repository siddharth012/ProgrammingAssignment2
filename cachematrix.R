## These functions calculate the inverse of a matrix. In order to decrease the number of computations,
## we cache this calculated inverse into a variable, and in case the matrix is needed to be inverted again
## we just refer to the previously calculated value.

## Two functions have been coded to perform this task. First function stores the initial matrix and its 
## inverted value using a pair of setter and getter functions for each of them. This function returns a special
## list through which we can access these setter and getter functions. 'x' in this function stores the actual 
## matrix and 'i' contains its inverse. 'set' sets 'x' as a specific valued matrix, 'get' returns this matrix, 
## 'setsolve' sets the inverse and 'getsolve' returns it.

makeCacheMatrix <- function(x = matrix()) {
        i=NULL;
        set = function(y){
                x <<- y
                i <<- NULL
        }
        get = function(){
                x
        }
        setsolve = function(sol){
                i <<- sol
        }
        getsolve = function(){
                i
        }
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function extracts the cached inverse of this matrix and checks whether its null or not. If not, then
## this value is returned, else the inverse is calculated and stored in the cache memory for future use using
## setsolve().

cacheSolve <- function(x, ...) {
        i = x$getsolve()
        if(!is.null(i)){
                message("getting cached matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setsolve(i)
        i
}