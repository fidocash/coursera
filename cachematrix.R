## Put comments here that give an overall description of what your
## functions do
##Below we have two functions that allow us to calculate the inverse of 
##a squared matrix using
##cache memory. Therefore we can minimize the waiting time to get the same output,
##using the data stored in the cache.


## Write a short comment describing this function
##this function allows to create a special vector, i.e a list, 
##containing 3 sub-functions, utilized in the second function.
##The set() function is useful for debugging, and it isn't used in the second function.
##Pay attention to the input of this function, i.e the matrix 'x'. The same 'x' is used to indicate 
##both a matrix in the first function, both a general object in the second. This could confuse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## Write a short comment describing this function
##this second function checks if the result has been stored in cache memory. If not,
##it will calculate the new result. The x in the function argument is the result of the previous function.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
