makeCacheMatrix <- function(x = matrix()) {
        
        inv = NULL
        set = function(y) {
                # The operators <<- and ->> are normally only used in functions, 
		## and cause a search to made through parent environments for an existing definition of the variable being assigned. 
		##If such a variable is found (and its binding is not locked) then its value is redefined,
		## otherwise assignment takes place in the global environment.
               
                x <<- y
                inv <<- NULL
        }
        get = function() x

        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv

	#Almost all lists in R internally are Generic Vectors, whereas traditional dotted pair lists (as in LISP) remain available 
	#but rarely seen by users (except as formals of functions).

	#The arguments to list or pairlist are of the form value or tag = value. The functions return a list 
	# or dotted pair list composed of its arguments 		
	##with each value either tagged or untagged, depending on how the argument was specified. 

        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##When you write a function using the three-dots, you have to pass it to some function ##
##Very often if you pass it to a particular function with flexibly.

cacheSolve <- function(x, ...) {
        
        
        inver = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inver)){
                # get it from the cache and skips the computation. 
                message("Getting cached data...  ")
                return(inver)
        }
        
        # otherwise, calculates the inverse 
        mat = x$get()
        inver = solve(mat, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inver)
        
        return(inver)
}


x = rbind(c(1, 2), c(3, 4))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m) # this will get the values from cache
