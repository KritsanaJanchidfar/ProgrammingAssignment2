## Cache the inverse of a matrix given that the input matrix is a square invertible matrix  


## Return a special object(list) that stores the input square invertible matrix and caches its' inverse
## It contains four functions, namely set,get,setInverse, and getInverse
## 1. set : to set(change) the value of the matrix and reset inverse to NULL
## 2. get : to get the value of the matrix
## 3. setInverse : to set the value of the inverse
## 4. getInverse : to get the value of the inverse

makeCacheMatrix <- function( x = matrix()){
		inverse <- NULL
		set<- function(y = matrix()){
			x <<- y
			inverse <<- NULL
		}
		get <- function() x
		setInverse <- function(inv) inverse <<- inv
		getInverse <- function() inverse
		list(set = set, get = get, 
			setInverse = setInverse, getInverse =getInverse)
}

## Inputs: the special object(list) and others arguments to pass to function solve()
## Outputs: the inverse matrix of the input matrix stored in the special object
## Fist, it attempts to look for the cached inverse. 
## If the cache is found(getInverse() is not NULL), it immediately returns the cached inverse.
## Otherwise, it retrives the stored matrix, solve for the inverse, and cache this back to the 
## special object for later use.

cacheSolve <- function(x,...){
		inv <- x$getInverse()
		if(!is.null(inv)){
			return(inv)
		}
		data <- x$get()
		inv <- solve(data,...)
		x$setInverse(inv)
		inv
}