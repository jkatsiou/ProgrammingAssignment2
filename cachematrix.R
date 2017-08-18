## The two functions work together in order to create, store, search a cache of matrices and return their inverses. 
## First function creates the cache (list containing the matrix,its inverse and a set of functions), while second retrieves 
## the inverese (if it exists) or If the inverse has not been previous calculated (m = null) it calculates, stores and displays it. 



## The makeCacheMatrix function, accepts as input a matrix and returns an object which is a list containing the input matrix (in variable x) as well      
## as  a set of functions which are used by function cacheSolve.  The functions included are: 

## set:it initializes values of x and m of its enclosing environment by assigning values from passed from cacheSolve function set 

## get:Returns to cacheSolve the matrix x 

## setinverse:Gets the inverse which was caclulated by cacheSolve and stores it in variable m of its enclosing environment (list created by makeCacheMatrix) enviroment 
 
## getinverse:returns the inverse of matrix x, that has been previously stored by the setinverse function. If m has not been previoulsy stored it returns the 
## initial value of m which has been set to null  


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

## The cacheSolve function attempts to find a if an inverse has been previously created for the matrix included in the list passed to x  
## by  invoking the  $getinverse() element.  If this  returns a  value different than null, then it returns the previoulsy calculated inverse, prints the
## message "getting cached data" and stops the execution. Otherwise it calculates the inverse, displays and passes it to the object (list) to be
## stored for future use. 

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
