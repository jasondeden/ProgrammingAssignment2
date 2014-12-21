##makeCacheMatrix creates a simple numeric matrix
##cacheSolve checks to see if this interted matrix exists
##and if so, retrieves the inverse

## creates a simple numeric matrix and stores it for future use

count = 0 ##Tracking mechanism to determine if function has been re-run

makeCacheMatrix <- function(x = matrix(1:4,2,2)) {
     
     mymatrix <<- x ##stores matrix outside of function
     
     count <<- count+1 ##number used to track changes
                       ##to matrix in future
     
     mymatrix  ##returns mymatrix

}


## checks to see if matrix exists
## checks to see if matrix has possibly changed via count
## checks to see if cacheSolve has been run since last matrix change
## if all records are current, simply returns prior result
## if matrix doesn't exist, runs makeCacheMatrix and inverses result
## if records not current, reruns solve on matrix

solvecount = 0
cacheMatrix = null

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     if(is.null(x)){   ##assumes mymatrix will be the x value
          makeCacheMatrix() ##runs the function to create a matrix
          cacheMatrix <<- solve(mymatrix)
          solvecount <<- count ##for tracking purposes, now up to speed
          cacheMatrix
          
     }
     
     if(is.null(cacheMatrix) || solvecount != count){ ##if first run
                                                      ##or makeCacheMatrix 
                                                      ##has run since
          cacheMatrix <<- solve(mymatrix)
          solvecount <<- count ##now up to speed again
          cacheMatrix
          
     }
     
     cacheMatrix ##If all criteria are met, just redisplay cached value
          
     
}
