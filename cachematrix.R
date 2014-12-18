## This seems a bit like cheating, but we can leverage the 
## fact that a variable in R can hold any type of object.
## That means the code provided in the example can be used,
## with just a few minor modifications.
## We use the <<- operator to assign a value to an object in 
## an environment that is different from the current environment.


## The makeCacheMatrix function, creates a special "vector", 
## which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of a matrix 
## created with the makeCacheMatrix function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

## Example:
## > my_matrix <- matrix(data = 1:4, nrow = 2, ncol = 2)

## Print the matrix
## > my_matrix                  
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## Use the makeCacheMatrix function to set the matrix
## > make_matrix = makeCacheMatrix(my_matrix)

## Call makeCacheMatrix get() to print the matrix
## > make_matrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## Call cacheSolve - the inverse is not there, so it is calculated
## > cacheSolve(make_matrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Call cacheSolve - this time the inverse is available, so return cache version
## > cacheSolve(make_matrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5