## The two functions work together in order to create, store, search a cache of matrices and return their inverses. 
## First function creates the cache (a list containing the matrix,its inverse and a set of functions), while second retrieves 
## the inverese (if it exists) or If the inverse has not been previous calculated (m = null) it calculates, stores and displays it. 




## The makeCacheMatrix function, accepts as input a matrix and returns an object which is a list containing the input matrix 
## (in variable x), the inverse of input matrix (in variable m), as well as a set of functions which are used by function cacheSolve.
## The functions included are: 

## The elements of the object (list) created are used from solveCache, either for the computation or storage of matrix inverse


## set:it initializes values of x and m of its enclosing environment (list created by makeCacheMatrix) 

## get:Returns to cacheSolve the matrix x 

## setinverse:Gets the inverse which was caclulated by cacheSolve and stores it in variable m of its enclosing 
##environment (list created by makeCacheMatrix) enviroment 
 
## getinverse:returns the inverse of matrix x, that has been previously stored by the setinverse function. If m has not been previoulsy
## stored it returns the initial value of m which has been set to null  



makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
	get <- function() x

        setinverse <- function(inverse) m <<- inverse

        getinverse <- function() m
        
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}






## In order cacheSolve to work, it has to receive as input a list that was previously created by the makeCacheMatrix function.

## The cacheSolve function attempts to find a if an inverse has been previously created for the matrix included 
## in the list passed to x by invoking the  $getinverse() element, which returns the stored inverse of the matrix. 

## If this  returns a  value different than null (which means that an inverse has been previously calculated for the matrix)
## then it returns the inverse, prints the message "getting cached data" and stops the execution. 

## Otherwise it calculates the inverse, displays and passes it to variable m of the (list) by invoking the $setinverse() element. 


cacheSolve <- function(x, ...) {
       
        m <- x$getinverse()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()

        m <- solve(data)

        x$setinverse (m)

        m
}


