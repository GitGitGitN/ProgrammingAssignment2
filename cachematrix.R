## This function creates a special "matrix" object that can cache its inverse.
## You can get the matrix itself by "get" function and its inverse by "getinverse".
## If you get null when you use "getinverse", set the inverse to the object by 
## "cacheSolve". 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" object. If the inverse
## is not null, this function return the cached inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x
        
        $setinverse(i)
        i
}
