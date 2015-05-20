#The first function, makecacheMatrix does the following:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix()){
        inv <- NULL #begins by setting the inverse to NULL as a placeholder for a future value
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }#defines a function to set the vector, x, to a new vector, y, and resets the inverse, inv, to NULL
        get <- function() x #returns the matrix, x
        setinv <- function(inverse) inv<<- inverse #sets inv to inverse
        getinv <- function() inv #returns the inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) # returns the 'special vector' containing all of the functions just defined 
}

#what this function does: computes, caches and returns matrix inverse
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)#take the inverse here using solve(x)
        x$setinv(inv)
        inv
}

#testing how these functions work

amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()#returns original matrix
cacheSolve(amatrix) #computes, caches and returns matrix inverse
amatrix$getinv()#returns matrix inverse
