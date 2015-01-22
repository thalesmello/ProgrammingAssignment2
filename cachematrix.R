## Creates a cached matrix object, which is capable of caching the
## result of its inverse. The calculation of the inverse matrix of
## this object should be done with the cachedSolve function

makeCachedMatrix <- function(matrix = matrix()) {
    inverse_cache <- NULL

    set <- function(value) {
        matrix <<- value
        inverse_cache <<- NULL
    }

    get <- function() matrix

    set_inverse_cache <- function(inverse) inverse_cache <<- inverse

    get_inverse_cache <- function() inverse_cache

    list(set = set,
         get = get,
         set_inverse_cache = set_inverse_cache,
         get_inverse_cache = get_inverse_cache)
}


## Calculates the inverse matrix of a cached matrix object.
## If the result isn't cached, it uses the solve function to
## calculate the the inverse matrix, then it caches the result
## in the cached matrix object

cachedSolve <- function(cached_matrix, ...) {
    ## Return a matrix that is the inverse of 'x'

    current_cache <- cached_matrix$get_inverse_cache()

    if(!is.null(current_cache)){
        return(current_cache)
    }

    matrix <- cached_matrix$get()
    inverse <- solve(matrix)
    cached_matrix$set_inverse_cache(inverse)

    inverse
}
