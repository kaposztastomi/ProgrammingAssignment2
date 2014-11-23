## These functions calculate inverse of a matrix 
## Similar to the assignment example R code contains 2 different functions 

## makeCacheMatrix implemets the setter and getter function and creates the variables in the appropriate environment 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
}


## cacheSolve checks the inverse of the matrix. cacheSolve calculates if it requested

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) 
        x$setinv(inv)
        inv
}


# a<-matrix(c(4,3,3,2), 2,2)
# mc<-makeCacheMatrix(a)
# cacheSolve(mc)