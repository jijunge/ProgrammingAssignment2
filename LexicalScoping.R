#Jijun Ge
#R programming, Fall 2015
#Programming Assignment 2: Lexical Scoping

##Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly (there are also 
##alternatives to matrix inversion that we will not discuss here). Your assignment 
##is to write a pair of functions that cache the inverse of a matrix.

#1.	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ##m is the cached variable. It is in the function but above subset
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(n) m <<- solve(n)
    getInverseMatrix <- function() m
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}
a <- matrix(rnorm(16),4,4)
a
makeCacheMatrix(a)
x <- makeCacheMatrix()

x$set(a)
x$get()

x$setInverseMatrix(a)

y <- x$getInverseMatrix()
y
a %*%y


#2.	cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
#not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getInverseMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m
}

a <- matrix(rnorm(16),4,4)
a
y<-makeCacheMatrix()
y$setInverseMatrix(a)
d <- cacheSolve(y)

a %*% d

