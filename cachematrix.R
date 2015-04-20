## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# makeCacheMatrix: This function creates a special
	# "matrix" object that can cache its inverse.
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	inverse_mat <- NULL
	
	# 1. Function for set matrix
	set <- function(y){
		# "<<-" is assign for global variables. Read more: ?assignOps
		x <<- y
		inverse_mat <<- NULL
	}
	
	# 2. Function for get matrix
	get <- function(){
		# Just return x
		x
	}
	
	# 3. Function for set inverse matrix
	setinverse <- function(solve){
		inverse_mat <<- solve
	}
	
	# 4. Function for get inverse matrix
	getinverse <- function(){
		inverse_mat
	}
	
	# return
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# cacheSolve: This function computes the inverse of the special
	# "matrix" returned by makeCacheMatrix above. If the inverse
	# has already been calculated (and the matrix has not changed),
	# then the cachesolve should retrieve the inverse from the cache.
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	inverse_mat <- x$getinverse()
	
	# If inverse_mat contains something, then simply output its contents
	if(!is.null(inverse_mat)){
		message("getting cached data")
		return(inverse_mat)
	}
	
	# Calculating inverse matrix
	# NOTE: We will not be able to get to this part of the code,
	# if higher work function return
	data <- x$get()
	inverse_mat <- solve(data, ...)
	x$setinverse(inverse_mat)
	# invisible return
	invisible(inverse_mat)
}

UnitTest <- function(){
	# I used this function to check my code
	
	# Vector for results of tests
	results <- c(FALSE, FALSE, FALSE)
	names(results) <- c("a = b$get? (CA: TRUE)",
		"First call of getinverse = NULL? (CA: TRUE)",
		"b$getinverse() = inverse a matrix (a_inv)? (CA: TRUE)")
		
	a <- matrix(1:4, 2, 2, byrow = TRUE)
	# a =
	#		1   2
	#		3   4
	b <- makeCacheMatrix(a)
	
	# a_inv is inverse a matrix. I use GNU Octave for found it:
	# a_inv =
	#	   -2.0  1.0
	#		1.5  0.5	
	a_inv = matrix(c(-2, 1, 1.5, -0.5), 2, 2, byrow = TRUE)
	
	# let's check our functions =)
	results[1] <- identical(a, b$get())
	results[2] <- is.null(b$getinverse())
	# calculate inverse matrix
	cacheSolve(b)

	# identical does not work :( see next code
	# results[3] <- identical(b$getinverse(), a_inv)
	# see lecture "Loop Functions - mapply" for details
	abs_diff <- function (x,y){
		abs(x - y)
	}
	results[3] <- (sum(mapply(abs_diff, b$getinverse(), a_inv)) < 1E-6)

	message("--------[ results of tests: ]--------------")
	print(results)
	message("* 'CA' mean Correct Answer")
	message("-------------------------------------------\n")
	message("Last check.\nYou must see message 'getting cached data' and inverse matrix:\n")
	
	cacheSolve(b)
}