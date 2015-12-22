## Matrix inversion usally costs alot
## so it's easier to cache the inverse
## rather than doing it repeatedly
## the two functions bellow cache a martix's inverse.

## makeCacheMartix create a list that constains
## a function that sets and gets
## the value of a matrix and its inverse
## the matrix and inverse
## each have there own set and get coding

makeCacheMatrix <- function(x = numeric()) {
	data <- NULL
	setM <-function(y) {
	x <<-  y 
	data <<-NULL
	}
		getM<- function() x			
		setI<- function(inverse) data <<- inverse
			getI <<- function()data	
			list(setM=setM, getM=getM, 
			setI=setI, getI=getI)
			}

## This next fuction returns the inverse
## of a martix and checks if the inverse
## has been computed. Then it gets the
## result and skips computing. But, if
## the inverse hasn't be computed.
## the function sets the value 
## in the cache.


	
## This function assumes that the matrix can always be inverted
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              data <- x$getI()
              if (!is.null(data)) {
              	message("getting cached data")
              	return(data)
              }
              
             inv <-x$getM()
              data <-solve(inv, ...)
              x$setI(data)
              data
              }
              
## Work sample of two formulas 
              
# e <- matrix(c(3,5,6,8), nrow=2, ncol=2)
# f <- makeCacheMatrix(e)

# f$getM()
#      [,1] [,2]
# [1,]    3    6
# [2,]    5    8

# f$getI()
# NULL

# cacheSolve(f)
# [,1] [,2]
# [1,] -1.3333333  1.0
# [2,]  0.8333333 -0.5

# cacheSolve(f)
# getting cached data
# [,1] [,2]
# [1,] -1.3333333  1.0
# [2,]  0.8333333 -0.5

# f$getI()
# [,1] [,2]
# [1,] -1.3333333  1.0
# [2,]  0.8333333 -0.5

