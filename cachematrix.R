## makeCacheMatrix function creates matrix and set matrix inverse
## Function takes matrix type argument, example: makeCacheMatrix(matrix(1:20,nrow=5,ncol=4))
makeCacheMatrix <- function(x = matrix()) { 
    Value_of_inverse <- NULL
    set_value_of_matrix <- function(y) {
        x <<- y
        Value_of_inverse <<- NULL 
    }
    get_value_of_matrix <- function() #get the given matrix: variable x
    {x}
    Inverse_Set <- function(inverse) 
    {Value_of_inverse <- inverse}
    Inverse_Get <- function() 
    {Value_of_inverse}
    list(set_value_of_matrix = set_value_of_matrix, 
         get_value_of_matrix = get_value_of_matrix, 
         Inverse_Set=Inverse_Set, Inverse_Get=Inverse_Get )
}

#cacheSolve calculates the matrix inverse and gives matrix that has inverses in it.
cacheSolve <- function(x, ...) {
    Value_of_inverse <- x$Inverse_Get()
    if(!is.null(Value_of_inverse)) { 
        #if value is not null: give message, and return calculated inverse
        message("getting cached data")
        return(Value_of_inverse)
    }
    data <- x$get_value_of_matrix()
    Value_of_inverse <- solve(data, ...) 
    #the standard R function for matrix inverse is solve()
    x$Inverse_Set(Value_of_inverse) #set calculated inverse to value_of_inverse
    Value_of_inverse
}
