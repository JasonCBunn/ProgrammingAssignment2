## Cache Matrix Inverse Functions
## Jason Bunn
## 19 May 2015

## These two functions take advantage of the lexical scoping within R
## to calculate and store the inverse of a square matrix.  The matrix is 
## assumed to be invertible.  If the inverse is already calcuated, the cached
## value is returned by the cacheSolve() function.

## Developed as part of the R Programming Coursera offering, JHU

## Revision History
## 19 May 2015- Forked from gitub and edited

## Original:   https://github.com/rdpeng/ProgrammingAssignment2



## makeCacheMatrix returns a list of functions to store and retrieve a 
## matrix and its inverse.  
makeCacheMatrix <- function(x = matrix()) {
            minv <- NULL
            set <- function(y) {
                    x <<- y
                    minv <<- NULL
            }
            get <- function() x
            setinverse <- function(inv) minv <<- inv
            getinverse <- function() minv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix operated on in 
## makeCacheMatrix.  If the inverse was already calculated, it
## returns the cached value.  Otherwise, the inverse is calculated and
## stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            minv <- x$getinverse()
            if(!is.null(minv)) {
                    message("getting cached inverse")
                    return(minv)
            }
            matrix <- x$get()
            minv <- solve(matrix, ...)
            x$setinverse(minv)
            minv
}