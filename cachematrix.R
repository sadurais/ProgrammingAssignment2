#
# @name         - makeCacheMatrix
# @description  - Accepts a regular "invertible" square matrix as input, wraps it in
#                 a function-closure containing the cached inverted matrix and returns
#                 a list-wrapper of public function interfaces
# @input        - An invertible, square matrix
# @returns      - A list of public interface functions: set, get, setInverted & getInverted
#                 hiding the closure-variables givenMatrix & invertedMatrix and the
#                 internal private functions: setMatrix, getMatrix, 
#                 setInverseMatrix and getInverseMatrix
#
makeCacheMatrix <- function(givenMatrix = matrix()) {
    invertedMatrix <- NULL  # The cached, inverted-matrix

    #Internal (ie., internal to this makeCacheMatrix() functon) private functions
    # setMatrix, getMatrix, setInverseMatrix & getInverseMatrix that are mapped
    # to external interface functions set, get, setInverse & getInverse respectively.
    setMatrix <- function(y) {
        # Y is the matrix given, first assign it to givenMatrix
        givenMatrix <<- y

        # Every time givenMatrix changes (is set),
        # the invertedMatrix needs to be reset to NULL
        # that would eventually force the 'cacheResolve' function to
        # recalculate the inverse and set it again via the 'set' interface
        invertedMatrix <<- NULL
    }
    getMatrix <- function() { givenMatrix }
    setInverseMatrix <- function(inverse) { invertedMatrix <<- inverse }
    getInverseMatrix <- function() { invertedMatrix }

    # External interface functions set, get, setInverse & getInverse
    list(set = setMatrix, get = getMatrix,
        setInverse = setInverseMatrix,
        getInverse = getInverseMatrix)
}


#
# @name         - cacheSolve
# @description  - Accepts the special wrapper-object (as returned by 'makeCacheMatrix' 
#                 function) as input and returns the inverted matrix. 
#                 If cached, returns the cached copy quickly.
# @input        - A wrapper-object (actually n invertible, square matrix
# @returns      - The cached inverted matrix. If cache had been invalidated (say 
#                 with a 'set' operation), recalculates the inverted-matrix, updates 
#                 the cache copy and then returns it
#
cacheSolve <- function(x, ...) {
    inverted <- x$getInverse()
    if(!is.null(inverted)) {
        message("getting cached inverted-matrix")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$setInverse(inverted)
    inverted
}


# Test a few test-cases
runTests <- function() {
    create_an_invertible_matrix <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

    origMat <- create_an_invertible_matrix(8)
    x = makeCacheMatrix(origMat) # 'x' is the special object containing both given and cached-inverted matrices

    invMat <- cacheSolve(x)   # You SHOULD NOT see the message: "getting cached inverted-matrix"

    #print("Original Matrix ="); print(origMat)
    #print("Inverted Matrix ="); print(invMat)
    #print("Inverted Matrix (from cache)="); print(invMat2)

    invMat2 <- cacheSolve(x)  # You SHOULD see the message: "getting cached inverted-matrix"
    print("are invMat and invMat2 identical? "); print(identical(invMat, invMat2));

    x$set(create_an_invertible_matrix(4)) # Change the givenMatrix effectively invalidating the cache
    invMat <- cacheSolve(x)   # You SHOULD NOT see the message: "getting cached inverted-matrix"
    invMat2 <- cacheSolve(x)  # You SHOULD see the message: "getting cached inverted-matrix"
    print("are invMat and invMat2 identical? "); print(identical(invMat, invMat2));
}

