
# The makeCacheMatrix will make a matrix that will then cache its inverse
# The argument 'input' is the initial matrix input from the user
library(MASS)
makeCacheMatrix <- function(input = matrix()) {
      inversematrix<- NULL                    # creates an 'inverse' object and sets it to NULL for now
      set<- function(y)   {               # function to set the value of the matrix
      input<<- y
      inversematrix<<- NULL
      }
      
      get<- function() {input}               # function to get the value of the matrix
      setinversematrix<- function(inverse) {inversematrix<<- inverse}    # setting the value of the inverse
      getinversematrix<- function() {inversematrix}               # getting the value of the inverse
            list(set=set, get=get,                          #return a list with the inverse of the matrix
                 setinversematrix= setinversematrix,
                 getinversematrix= getinversematrix)
}


# The cacheSolve function calculates the inverse of the matrix created by makeCacheMatrix.
# If the inverse of the matrix has been calculated, it will get this from the cache

cacheSolve <- function(input, ...) {
      inversematrix <- input$getinversematrix()  # returns a matrix that is the inverse of the inputted matrix
      if (!is.null(inversematrix)) {               # if inverse has already been calculated
            message("getting cached data")
            return(inversematrix)     # return the cached value of the inverse of the original matrix
      }
      newmat<- input$get()    # otherwise compute the inverse and set its value in the cache
      inversematrix<- solve(newmat,...)
      input$setinversematrix(inversematrix)  #setting the value of the inverse in the cache
      inversematrix                # return the initial matrix
}
