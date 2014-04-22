## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        # initializes the stored inverse value to NULL
        inverse <- NULL
        
        # sets the initial value of the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL    # resets the inverse variable to NULL since the matrix changed
        }
        # gets the value of the matrix
        get <- function() x
        # the setinverse function sets the inverse
        setinverse <- function(inv) inverse <<- inv
        # and this gets the inverse
        getinverse <- function() inverse
        
        # returns a list of the above functions 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function does the calculations for the inverse of the
## matrix created with makeCacheMatrix.
## It starts by checking to see if the inverse has already been calculated
## If it has been calculated, it pulls the inverse from the cache and skips any calculation.
## If it hasn't, it calculates the inverse of the matrix and then sets the value of the inverse
## in the cache.

cacheSolve <- function(x, ...) {
        # this checks to see if the inverse is already cached
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # if it is not cached, we set the matrix into data
        data <- x$get()
        # this computes the inverse
        inverse <- solve(data)
        # this caches the inverse
        x$setinverse(inverse)
        # and finally returns the inverse
        inverse
}
