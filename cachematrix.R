## The two functions work together in order to create, store, search a cache of matrices and return their inverses. 
## The makeCacheMatrix creates the cache (a list containing the matrix,its inverse and a set of functions), while 
## cacheSolve retrieves the inverese (if it has been previously calculated) otherwise it calculates, stores and displays
## the inverse 



## The makeCacheMatrix function: 

## The makeCacheMatrix function, accepts as input a matrix and returns an object which is a list containing the input matrix  
## (in variable x), the inverse of input matrix (in variable m), as well as a set of functions which are used by functions.
## The functions of the list are used from cacheSolve, for the computation or storage of matrix inverse

## The functions included in the list are: 


## set:it initializes values of x and m of its enclosing environment (list created by makeCacheMatrix) 

## get:Returns to cacheSolve the matrix x 

## setinverse:Gets the inverse which was caclulated by cacheSolve and stores it in variable m of its enclosing 
##environment (list created by makeCacheMatrix)
 
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



## The cacheSolve Function :

## In order cacheSolve to work, it has to receive as input a list that was previously created by the makeCacheMatrix function.

## The cacheSolve function attempts to find if an inverse has been previously created for the matrix included 
## in its argument (list x), by invoking the  $getinverse() element of the list and storing the result in local variable m

## If $getinverse returns a  value different than null (which means that an inverse has been previously calculated for the matrix)
## then it returns the inverse, prints the message "getting cached data" and stops the execution. 

## Otherwise it calculates the inverse, displays and passes it to variable m of the list by invoking the $setinverse() element. 


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


