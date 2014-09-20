## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(y){
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        set_inverse_matrix <- function(inverse_matrix) inverse_x <<- inverse_matrix
        get_inverse_matrix <- function() inverse_x
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$get_inverse_matrix()
        if (!is.null(inverse_matrix)){
                return(inverse_matrix)
        }
        matrix <- x$get()
        inverse_matrix <- solve(matrix)
        x$set_inverse_matrix(inverse_matrix)
        inverse_matrix
}
