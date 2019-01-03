
# Summary -----------------------------------------------------------------


# Below are two functions that are used to create a special object that stores a
# matrix and cache's its inverse.


# makeCacheMatrix ---------------------------------------------------------

# The first function, makeCacheMatrix creates a special matrix, which is a list
# of functions to set and get the value of the matrix and to set and get the
# value of the matrix' inverse


makeCacheMatrix <- function(x = matrix()) {             # initializing the matrix
        i <- NULL                                       # setting the inverse i to NULL 
        set <- function(y) {                            # setter function
                x <<- y                                 # Assign y, the input argument, to x (the matrix) in the parent environment 
                i <<- NULL                              # Assign value NULL to i in the parent environment (clearing of value i previously cached by a prior execution of chacheSolve().)
        }                                               # If there is already a cached i, whenever x is reset, the value of i cached in the memory of the object is cleared, forcing recalculation of the inverse.
        get <- function() x                             # getter function: x is retrieved from parent environment 
        setinv <- function(inverse) i <<- inverse       # assigns inverse to i in parent environment
        getinv <- function() i                          # retrieving of value of i from parent environment
        list(set = set, get = get,                      # list created with elements consisting of the functions,which is returned to parent environment
             setinv = setinv,
             getinv = getinv)

}


# chacheSolve -------------------------------------------------------------


# cacheSolve calculates the inverse of the above created matrix by first
# checking whether the inverse has already been calculated. If it was created
# before it retrieves the inverse from the cache and skips further caluclation.
# Otherwise, it calculates the inverse and sets the value of the inverse in the
# cache by using the setinv function.

cacheSolve <- function(x, ...) {                        # ellipsis allows to pass additional arguments
        i <- x$getinv()                                 # function retrieves inverse passed as argument
        if(!is.null(i)) {                               # checks wehter result is NULL
                message("getting cached data")          # if FALSE, i.e. i=NULL ...
                return(i)
        }
        data <- x$get()                                 # get data
        i <- solve(data, ...)                           # calcualte inverse
        x$setinv(i)                                     # sets inverse on input matrix
        i ## Return a matrix that is the inverse of 'x'
}
