
#########################################################################
#Recommend removing existing objects in the workspace using the following 

rm(list=ls())



#########################################################################
# This function creates a special "matrix" (called x_inv) object 
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	
	# finds the size of the square matrix	
	size<-mean(dim(x))
	
	# makes a matrix of ones(1)
	holder<-matrix(1,nrow=size,ncol=size)

	# passes the matrix out of the function (cache)
	x_inv<<-holder

}


#########################################################################
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache


cacheSolve <- function(x, ...) {
 
# Check that this hasn't been solved already for the existing matrix
# There are two ways to check this:
#	Option 1: det(x_inv)=1/det(x)
#	Option 2: x_inv*x=Identity matrix
# We'll pick option 2 because I think it's quicker

# finds the size of the square matrix	
size<-mean(dim(x))

	if(trunc(sum(sapply(x_inv%*%x,sum)),1) == size){

		# If the matrix multiplication results in the identity matrix 
		# the we'll get the number of rows (or collumns) to be the same
		# as the sum of all of the values in the matrix 

		# If it has been solved already then don't re-calculate,
		# just report back the value that was saved

		print("Cached result")
		x_inv


	} else{

	# If it has not been solved already then do re-calculate
	print("Calculated result")
	x_inv<<-solve(x)
	x_inv


	}
}




