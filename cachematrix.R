# These two functions are used to compute the inverse of a matrix and cache the inverse by
# using "<<-" operation (creating an object that can be identified across environments).

# This function is created to set a matrix, get a matrix, set the inverse of the matrix, 
# and get the inverse of the matrix. 

makeCasheMatrix <- function(x = matrix()) {	# create a function for a matrix
        m <- NULL	# create an object m to store the inverse and the default is NULL
        set <- function(y) {	# reset the matrix and the cached inverse to be NULL.
                x <<- y
                m <<- NULL
        }
        get <- function() x	# get the current matrix
        setInverse <- function(matrixInverse) m <<- matrixInverse  
        getInverse <- function() m  # get the inverse of the matrix
        list(set = set, get = get,  # create the list containing the functions above
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function computes the inverse of the matrix returned by function makeCasheMatrix. 
# If the inverse of an matrix has been calculated, then the cached inverse will be retrieved. 
 
cacheSolve <- function(x, ...) {  
        m <- x$getInverse()	# retrieve the inverse in the cache
        if(!is.null(m)) {	# if the cached inverse is not NULL, then output the message 
        					# "getting cached data" and return the inverse.
                message("getting cached data")
                return(m)	# return to skip recalculation of the inverse
        }
        data <- x$get()	# if the cached inverse is NULL, then get the current matrix and 
        				# compute the inverse
        m <- Solve(data, ...)	# compute the inverse
        x$setInverse(m)  # set the value of the inverse
        m	# return the calculated inverse
}