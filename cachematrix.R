## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## The following pair of functions are created to cache a special matrix object 
## that can cache its inverse or computes the inverse of the special "matrix".


## 1. "makeCacheMatrix" function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                       ## create an empty inverse property
        set <- function(y) {            ## 1. set the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x             ## 2. get the matrix
        setInverse <- function(inverse) i <<- inverse   ## 3. set the inverse of the matrix
        getInverse <- function() i      ## 4. get the inverse of the matrix
        
        ## Return the list 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## 2. "cacheSolve" function computes the inverse of the special "matrix" returned 
## by function "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then function "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        ## if the inverse has already been calculated
        if(!is.null(i)) {      
                message("getting cached an inverse of matrix")
                return(i)
        }
 
        ## if the inverse have not been calculated
        mat <- x$get()          ## get the matrix
        i   <- solve(mat, ...)  ## inverse the matrix
        x$setInverse(i)         ## set the inverse of the matrix
        i                       ## return the inverse of the matrix
}

## Test Functions
## 1. Create a matrix for testing
# > testMatrix <- matrix(c(1,4,9,16), nrow=2, ncol=2)
# > MCM <- makeCacheMatrix(testMatrix)
# > MCM$get()
#      [,1] [,2]
# [1,]    1    9
# [2,]    4   16

## 2. When the inverse have not been calculated or cached, 
##    "cacheSolve" function computes the inverse of the matrix
# > MCM$getInverse()
# NULL
# > cacheSolve(MCM)
#      [,1]  [,2]
# [1,] -0.8  0.45
# [2,]  0.2 -0.05

## 3. When the inverse has already been calculated, 
##    return inverse of matrix with message: getting cached an inverse of matrix
# > MCM$getInverse()
#      [,1]  [,2]
# [1,] -0.8  0.45
# [2,]  0.2 -0.05
# > cacheSolve(MCM)
# getting cached an inverse of matrix
#      [,1]  [,2]
# [1,] -0.8  0.45
# [2,]  0.2 -0.05
