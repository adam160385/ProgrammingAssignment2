## The function "makeCacheMatrix" works in a analogous way as
## the mean-function given in the example, so it creates a
## list containing a function to set the value of the matrix, 
## get the value of the matrix, set the value of inverse of the
## matrix and get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inver <<- inverse
    getinv <- function() inver 
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The next function assumes that the matrix is always invertible.
## It returns the inverse of the matrix, by first checking if
## the inverse has already been computed, ang getting the results
## and skipping computation, or otherwise it inverts the matrix.

cacheSolve <- function(x, ...) {
    inver <- x$getinv()
    if(!is.null(inver)) {
        message("getting cached data.")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinv(inver)
    inver
}

## Examplary results:
## > x <- cbind(1:3, c(1.5, 3.4, 2.8), c(6.1, 7.5, 4.2))
## > x
##      [,1] [,2] [,3]
## [1,]    1  1.5  6.1
## [2,]    2  3.4  7.5
## [3,]    3  2.8  4.2
## 
## > z = makeCacheMatrix(x)
## > z$get()
## 
##      [,1] [,2] [,3]
## [1,]    1  1.5  6.1
## [2,]    2  3.4  7.5
## [3,]    3  2.8  4.2
## 
## When I run the function for the first time, no cache is found.
## > cacheSolve(z)
##            [,1]       [,2]        [,3]
## [1,]  0.4930301 -0.7909024  0.69625825
## [2,] -1.0344828  1.0344828 -0.34482759
## [3,]  0.3374908 -0.1247249 -0.02934703
## 
## However, in the second run the function used cache.
## > cacheSolve(z)
## getting cached data.
##           [,1]       [,2]        [,3]
## [1,]  0.4930301 -0.7909024  0.69625825
## [2,] -1.0344828  1.0344828 -0.34482759

