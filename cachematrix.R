## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # Initialise the inverse matrix
        inv <- NULL 

        # Set the value of the matrix 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # Get the value of the matrix 
        get <- function() x

        # Set the inverse matrix
        setInv <- function(InvMatrix) inv <<- InvMatrix

        # Get the inverse matrix
        getInv <- function() inv

        list(set = set,
            get = get,
            setInv = setInv,
            getInv = getInv)
}


## Compute the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
        # If inverse exists, return the value
        invM <- x$getInv()
        if (!is.null(invM)){
                print("Getting cached data")
                invM
        }

        # Else, get the matrix and compute the inverse.
        invM <- solve(x$get(),...)

        # Set the inverse in the cache
        x$setInv(invM)

        # Return the computed inverse.
        print('Computing inversed matrix')
        invM
}
