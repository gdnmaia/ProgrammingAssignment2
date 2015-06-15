# Assigment 2 - R Programming - Coursera 
## Instructor: R. D. Peng - June 2015


# makeCacheMatrix

## First object of the list (function 'setMatrix'): 
        ## replaces matrix previously defined with the makeCacheMatrix function
        ## and makes 'x' available to be used by functions in other environments

## Second object of the list (function 'getMatrix'): 
        ## displays the current matrix (created from 'makeCacheMatrix' or 'set')

## Third object of the list (function 'setInverseMatrix'): 
        ## only active if matrix is cached

## Fourth object of the list (function 'getInverseMatrix'): 
        ## only active if matrix is cached; otherwise NULL

### The thrird and fourth list objects communicate with 'cacheSolve' 
###function via the superassignment call ('<<-').

makeCacheMatrix <- function(x = matrix()) {
        
        inverseMatrix <- NULL       
                
        list    (
                
                setMatrix = setMatrix <- function(y) {                    
                                        x <<- y                         
                                        inverseMatrix <<-  NULL         
                                },
                
                getMatrix = getMatrix  <- function() x,
                
                setInverseMatrix = setInverseMatrix <- function(solve) 
                                                inverseMatrix <<- solve,
                
                getInverseMatrix = getInverseMatrix <- function() inverseMatrix
                
                )
        
} 


## Write a short comment describing this function
## It cashes the inverse of the matrix

cacheSolve <- function(x, ...) {
        
                inverseMatrix <- x$getInverseMatrix()
        
                if(!is.null(inverseMatrix)) {
                        message("getting cached data")
                        return(inverseMatrix)  
                }
                        
                data <- x$getMatrix()
        
                inverseMatrix <- solve(data, ...)
        
                x$setInverseMatrix(inverseMatrix)
        
                inverseMatrix
}
