## cachematrix.R by jjperez
##
## This set of functions allow you to create a vector of functions and cache the inverse of a given matrix to be reused.4
## While makeCaheMatrix generates a vector of functions, cacheSolve calls that vector to check if there is previous matrix
## storaged on the cache and, if so, it prints it. Otherwise it calculate the new inverse matrix and storage it on the cache.

## makeCacheMatrix v.1.0 Apr-2015

## makeCacheMatrix create a vector containing four functions: set, get, setsolve and getsolve
## 1. set. storage the matrix given as data on x.
## 2. get. print the matrix storaged on x.
## 3. setsolve. storage the given value on s variable
## 4. getsolve. print the value storaged on s.

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function (y){
          x <<- y
          s <<- NULL
        }
        get <- function () x
        setsolve <- function (solve2) s <<- solve2
        getsolve <-  function () s
        list (set = set, get = get, setsolve=setsolve, getsolve=getsolve)
  
}


## cacheSolve v1.0 Apr-2015

## cacheSolve return the inverse matrix of a given one. cacheSolve first check the cache to check if the inverse
## matrix has been calculated before. If so, it returns the storaged value, if not it calculates the inverse matrix,
## cache the value and return it.

## How to use it:
## 1. create the function vector: x<-makeCacheMatrix()
## 1. storage the matrix to be inverted on a variable: x$set(a)
## 2. call the cacheSolve function to get the inverted matrix: cacheSolve(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <-x$getsolve()
        if(!is.null(s)){
          message("getting cached data")
          return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
