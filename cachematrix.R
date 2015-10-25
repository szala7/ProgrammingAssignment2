## Caching the inverse of a matrix (used the solve() function),
## it can create and use the inverted matrix (with the caching ability).


## Creates a cacheable matrix (which is a list) to the cacheSolve() function,
## which function can set and get the cached values from.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL

        ### set the matrix value
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ### get the matrix value
        get <- function() x
        
        ### set the inverse matrix value
        setinv <- function(solve) inv <<- solve
        
        ### get the inverse matrix value
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Compute or cache the inverse of a cacheable matrix returned the makeCacheMatrix()


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ### if it can be cached
        inv <- x$getinv()
        
        if(!is.null(inv)){
                message("get inverse from cached data")
                return(inv)
        }
        
        ### if it can't be cached
        mtx <- x$get()          #get it
        inv <- solve(mtx, ...)  #solve it
        x$setinv(inv)           #set it
        inv                     #return
}
