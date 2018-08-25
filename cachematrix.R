## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getmatrix()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setmatrix(m)
                m
}



size <- 10                                       # create test data (square matrix)
mydata <-matrix(rnorm(size*size), size, size) # a matrix likes this works too:  matrix(c(-1,1,3/2,-1), nrow=2, ncol=2)
mydata
mat <- makeCacheMatrix()                         # create caching data structure 
mat

mat$set(mydata)                                  # place data into structure
mat$getmatrix()                              # get inverse (empty at this point)
imat <- cacheSolve(mat)                          # calculate the inverse
imat
imat_cached <- cacheSolve(mat)                   # caculate the inverse again (retrieved cached value)
imat_cached

identical(imat, imat_cached)                     # verify inverse and cached inversed are the same
test <- mydata %*% imat                          # verify matrix %*% inverse = identity matrix
test

solve(mydata)
