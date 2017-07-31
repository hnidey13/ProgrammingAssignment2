#Two functions that allow to cache the inverse of a given matrix

#makeCacheMatrix creates a special "matrix", which is really a list containing a function to:

#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix

makeCacheMatrix <- function(M = matrix()) {
        inverse <- NULL
        set <- function(Y) {
                M <<- Y
                inverse <<- NULL
        }
        get <- function() M
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(M, ...) {
        inverse <- M$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- M$get()
        inverse <- solve(matrix, ...)
        M$setinverse(inverse)
        inverse
}

#Example of how to use the functions
B<-makeCacheMatrix(matrix(c(2,5,1,3),2,2))
B$get()
B$getinverse()
B$set(matrix(c(1,1,1,2),2,2))
B$get()
cacheSolve(B)
B$getinverse()
cacheSolve(B)