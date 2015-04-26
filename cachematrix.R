## Put comments here that give an overall description of what your functions do:
## the two functions makeCacheMatrix and cacheSolve are going by pair to cache the inverse of a matrix.

## Write a short comment describing this function:
## makeCacheMatrix stores 4 subfunctions : get (without argument, get the value of the stored matrix); 
## set (change the value of the stored matrix and reinitialize to NULL the stored inverse matrix);
## getsolve (without argument, get the value of the stored inverse matrix);
## setsolve (change the value of the stored inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}

## Write a short comment describing this function:
## cacheSolve checks the value of the stored inverse matrix (x$getsolve);
## if the value is not NULL (i.e., if the inverse matrix was already computed), it returns the value of the stored inverse matrix;
## else it computes the invesre matrix and change it stored value (through x$setsolve).

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting inverse of x")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## Test
Mat1 <- matrix(4:7,2,2)
a <- makeCacheMatrix(Mat1)
cacheSolve(a)
cacheSolve(a)
Mat1 <- matrix(1:4,2,2)
a$set(Mat1)
cacheSolve(a)