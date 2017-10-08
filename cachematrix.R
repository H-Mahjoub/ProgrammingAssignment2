## CacheMatrix.R file contains two functions, makeCacheMatrix() and cacheSolve().
## The first function in the file, makeCacheMatrix() creates an R object that
## stores a vector and its Solve (=inverted matrix). It is just like a storage.
## but not a calculator of Solve. it returns 4 functions within a list to
## the parent environment.Actually it uses "getters and settters" method which
## is more formally known as mutator and accessor methods.

## The second function, cacheSolve() requires an argument that is returned by
## makeCacheMatrix() in order to retrieve the Solve from the cached value
## that is stored in the makeCacheMatrix() object's environment. If there is no
## cached value from previous function, it will calculate the Solve from
## scratch.




## The makeCacheMatrix function takes x object as a function argument which is 
## defined as an empty matrix. The function initializes m whitin its environment
## with NULL value. m is a variable which is going to contain the value
## of inverted matrix (Solve)
## The function uses mutator and accessor method and results in a list with 4 
## function:
## 1) set(): it takes y variable as an input for assigning it to x and make m 
## NULL in parent environment (which is makeCacheMatrix environment) using <<- 
## operator. The set() is specially useful when we us "put in" method .
## 2) get(): it takes x
## 3) setsolve(): it assigns the  input argument to the value of m in the
## parent environment (which is makeCacheMatrix environment)
## 4) getsolve(): it returns m value (m is containing inverted matrix)

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


## cacheSolve() is required to calculate or retrieve the Solve from an object
## of type makeCacheMatrix(). It has a single argument, x, and an ellipsis.
## first it uses getsolve() from previous functon to test whether m is null or not.
## if m has value it returns its value from cache. otherwise it calculates it and
## assigns it to m. using setsolve() make it possible to cache its value for 
## future calculation.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m 
    
}
