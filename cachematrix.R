## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#steps provided in assignment:(matrix replaced value and inverse replaced mean)
#example in assignment:
#m <- NULL but in this case inverse will replace m
#set <- function(y) {
#x <<- y
#m <<- NULL
#}
#get <- function() x
#setmean <- function(mean) m <<- mean
#getmean <- function() m
#list(set = set, get = get,
#setmean = setmean,
#getmean = getmean)

makeCacheMatrix <- function(x = matrix()){
        
#m <- NULL but in this case inverse will replace m
        inverse <- NULL
#set the matrix of the vector
        set <- function(y) {
                x <<- y
               inverse <<- NULL
        }
#get the matrix of the vector
               get <- function() x
#set the inverse of matrix                
               setinverse_a <- function(inverse_a) inverse <<- inverse_a
#get the inverse of matrix              
               getinverse_a <- function() inverse
               #make it into list
               list(set = set, get = get,
                    setinverse_a = setinverse_a,
                    getinverse_a = getinverse_a)
        }

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse <- x$getinverse_a()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }   
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse_a(inverse)
       
}
#test
#x=matrix(1:4,2)
#x1=makeCacheMatrix(x)
#cacheSolve(x1)
#cacheSolve(x1)

