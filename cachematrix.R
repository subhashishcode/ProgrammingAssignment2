## The two functions between themselves optimize matrix inverse calculation.
## makeCacheMatrix caches the inverse of a matrix that is passed to it. 
## The cached inverse is stored in a scope that makes it accessible by clients
## of the makeCacheMatrix function.
## The cacheSolve function returns the inverse of the matrix that is passed
## to it. Instead of calculating the inverse always, it first uses the
## makeCacheMatrix function to check if the inverse of the concerned matrix
## is cached and if yes, returns that. If not, it calculates the inverse
## and caches it using makeCacheMatrix

## This function caches the inverse of the matrix passed to it

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)  {
        x <<- y
        inv <<- NULL
    }
    get <- function()   {
        return(x)
    }
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function()    {
        return(inv)
    }
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function returns the inverse of the matrix passed to it

cacheSolve <- function(x, ...) {
    funcs <- makeCacheMatrix(x,...)
    inv <- funcs$getinverse()
    if(!is.null(inv))   {
        print("returning cached data")
        return(inv)
    }
    matrix <- funcs$get()
    inv <- solve(matrix,...)
    funcs$setinverse(inv)
    return(inv)
}
