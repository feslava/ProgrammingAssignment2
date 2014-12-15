## This function creates a special "matrix" object that can cache 
## its inverse
makeCacheMatrix <- function(x = matrix()) 
{
    ## variable where store the matrix inverse
    ## by defualt, the inverse is NULL
    i <- NULL
    
    ## defines the matrix setter function. 
    ## actions:
    ##  (1) stores the new matrix
    ##  (2) removes the inverse of the previous matrix 
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    
    ## defines the matrix getter function
    ## actions: 
    ##  returns the current matrix
    get <- function() x
    
    
    ## defines the inverse setter function. 
    ## actions:
    ##  stores the new inverse of the matrix
    setInverse <- function(inverse) i <<- inverse
    
    ## defines the inverse getter function. 
    ## actions:
    ##  returns the inverse of the matrix
    getInverse <- function() i
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
    ## takes the current matrix inverse
    i <- x$getInverse()
    
    ## verifies the inverse in order to skip recalculation
    if (is.null(i))
    {
        ## if the invserse is NULL, it hasn't been
        ## calculated previously.        
        ## we need to calculate and store it.
        ## 3 actions to do:
        
        ##  (1) takes the current matrix
        m <- x$get()
        
        ##  (2) calculates the inverse
        i <- solve(m)
        
        ## (3) stores the inverse of the matrix in order to 
        ## skip future calculations
        x$setInverse(i)
    }
    else
    {
        ## it's in cache, nothing to do
    }
   
    ## returns a matrix that is the inverse of 'x'
    return(i)
}
