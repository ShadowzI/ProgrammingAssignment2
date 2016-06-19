
##Function creates a matrix object that caches its inverse 

makeCacheMatrix <- function(x = matrix())
{
##makeCacheMatrix creates a list containing a function to set and get the value of the matrixs and inverse matrixs 
        
        inv <- NULL
        ##This is initially set to NULL until the user sets the value

        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        ##the set function sets the matrix itself not including the inverse matrix
   
        get <- function() x
        ##the get function gets the matric itself not including the inverse matrix
        
        setinverse <- function(inverse) inv <<- inverse
        ##This manually sets the inverse matrix
        
        getinverse <- function() inv
        ##this gets the inverse matrix
   
        list
        (   
                set=set, 
                get=get, 
                setinverse=setinverse, 
                getinverse=getinverse
        )
        ##Encloses everything into a list
}
 
##Using the matrix returned from the former makeCacheMatrix, the inverse of said matrix is computated

cacheSolve <- function(x, ...)
{
##The matrix of the inverse of "x" is returned here

        inv <- x$getinverse()
        ##The inverse is checked to see if its been computated 
   
        if(!is.null(inv))
        {
                message("Cached data is:")
                return(inv)
        }
        ##Continuing from here this if statement tells it to simply return the computed inverse if it has already been computated
   
        data <- x$get()
        ##In the case it hasn't, the matrix itself is gotten
        
        inv <- solve(data)
        ##The inverse is then found
        
        x$setinverse(inv)
        ##and this result is cached in the object
        
        inv
        ## The new result which is the inverse of "x" is returned
}
