## Put comments here that give an overall description of what your
## functions do
## This program contains two functions "makeCacheMatrix"
## and "cacheSolve", that cache or solve the inverse of a matrix


## Write a short comment describing this function
## makeCacheMatrix is a function that creates a special "matrix"
## object that can cache its inverse for the input (assuming that 
## the matrix supplied is always invertible)
## Matrix_inv : Matrix inverse
## Inv: inverse

makeCacheMatrix <- function(x = matrix()) {
        Matrix_inv <- NULL
        set <- function(y){
                x <<- y
                Matrix_inv  <<- NULL
        }
        get <- function()x
        setMatInv <- function(inv) Matrix_inv <<- inv
        getMatInv <- function() Matrix_inv
        list(set = set, get = get, 
             setMatInv = setMatInv,
             getMatInv = getMatInv)
}

## Write a short comment describing this function
## cacheSolve functions checks if the cache already has the
## inverse of a matrix, if not it computes and returns 
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Matrix_inv <-x$getMatInv()
        if(!is.null(Matrix_inv)) {
                message("getting cached data")
                return(Matrix_inv)
        }
        data <- x$get()
        Matrix_inv <- solve(data, ...)
        x$setMatInv(Matrix_inv)
        Matrix_inv
}

