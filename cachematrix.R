## makeCacheMatrix is a function for working with matricies which cache the 
## operations results
makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize new matrix for cache operations
        setMatrix <- function (y) {
                matrix <<- y
                inverse << NULL
        }
        
        getMatrix <- function (y) {
                return (matrix)
        }
        
        getInverse <- function (y) {
                return (inverse)
        }
        
        setInverse < function (y) {
                inverse <<- y
        }
}



## cacheSolve - calculates the inverse martix which was created using
## makeCacheMatrix for cached operations. 
## It solves the matrix and stores ther result in cache. Further calls retreive
## the inverse matrix from cache and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # try to get the previously calculated inverse
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                # return the cached data
                return (inverse)
        }
        
        # otherwise calculate the inverse and store it for later usage
        matrix <- x$getMatrix()
        inverse <- solve(matrix)
        x$setInverse (inverse)
        return (inverse)
}
