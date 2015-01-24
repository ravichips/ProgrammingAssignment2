## R - ProgrammingAssignment #2
## Pair of functions meant to showcase the capability to cache data within functions

## Function object that caches a matrix and its corresponding inverse
## Returns pointers to getters and setters to access cached matrices
makeCacheMatrix <- function(sourceMatrix = matrix()) {

    ## Formal param: sourceMatrix, falls is within the scope of this function
    ## invertedMatrix: falls is within the scope of this function

    invertedMatrix <- NULL

    ## Returns the current(cached) source matrix
    getMatrix <- function() {
        sourceMatrix
    }

    ## Sets a new source matrix
    setMatrix <- function(newMatrix) {
        ## Formal param: newMatrix falls within the scpe of this "set" func enclosure
        ## Params: sourceMatrix and invertedMatrix, are defined outside the scope of this func
        ## Hence, both the variables are accessed via the <<- operator
        sourceMatrix <<- newMatrix

        ## When a new sourceMatrix is set, we need to reset the corresponding invered matrix to NULL
        ## thus enforcing a recomputation in cacheSolve
        invertedMatrix <<- NULL
    }


    ## Returns the cached inverted matrix
    getInvertedMatrix <- function() invertedMatrix

    ## Sets a new inverted matrix.  The variable we want to affect is defined in the enclosing func,
    ## hence, need to use op: <<-
    setInvertedMatrix <- function(iMatrix) {
        invertedMatrix <<- iMatrix
    }

    ## Return value from this func...a list with pointers to getters and setters of cached matrices
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix)
}

## Func returns an inverted matrix corresponding to a source matrix
## thats accessed via the functions listed in formal param: matrixAccessFuncList.
## Returns a cached vrsion of the inverted matrix, if available.
## Otherwise, computes, sets the inverted matrix in the cache & returns it ben do
cacheSolve <- function(matrixAccessFuncList) {

    ## Get the cached inverted matrix
    invMatrix <- matrixAccessFuncList$getInvertedMatrix()

    ## Self explanatory?!!
    if(!is.null(invMatrix)) {
        message("Returning cached inverted matrix.")
        return(invMatrix)
    }
    print("Cached inverted matrix not found....computing a new inverted matrix instance....")

    ## Get the source matrix
    srcMatrix <- matrixAccessFuncList$getMatrix()

    ## Compute matrix inverse
    invMatrix <- solve(srcMatrix)

    ## Set new inverted matrix back in cache
    matrixAccessFuncList$setInvertedMatrix(invMatrix)

    ## Return inverted matrix
    invMatrix
}