## This function creates a matrix that can cache its inverse using several functions.

## makeCacheMatrix creates mat (where the inverse matrix will be stored), get will return the original matrix, 
## setmat will make the inverse matrix, and getmat returns the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function()x
        setmat <- function(solve) mat <<- solve
        getmat <- function()mat
        list (set = set, get = get,
                setmat = setmat,
                getmat = getmat)
}


## cacheSolve will solve the inverse of the matrix created in makeCacheMatrix. First, it gets the inverse matrix
## if it is not caclucated, it will be null.  If it is not null, it will return the caculated inversion, otherwise,
## it will be solved.

cacheSolve <- function(x, ...) {
        mat <- x$getmat()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setmat(mat)
        mat
}
