##################################################################################
##################################################################################
# Coursera R Programming, Assignment 2, 07/22/2014
# Cache Inverse of Matrix
# Overall description: this pair of functions "makeCacheMatrix()" and "cacheSolve"
# can cache inverse of matrix, a potentially time-consuming computations
# by using lexical scoping rule of R language.   
##################################################################################
##################################################################################


## The "makeCacheMatrix" function creates a special matrix "x", that is a list containing a fucntion to:
## 1. set value of matrix
## 2. get value of matrix
## 3. set value of inverse of matrix
## 4. get value of inverse of matrix  

makeCacheMatrix <- function(x = matrix()) {
		m<-NULL
		set<-function(y){
				x<<-y
				m<<-NULL
		}
		get<-function()x
		setSolve<-function(Solve)m<<-Solve
		getSolve<-function()m
		list(set=set, get=get,
			setSolve=setSolve,
			getSolve=getSolve)

}


## The "cacheSolve" function returns inverse matrix of the special matrix "x" created from the above function.
## However, it first check if the inverse has been calculated. If so, it gets the value from cache 
## rather than re-computation. Otherwise, it computes the inverse and sets the value via setSolve() function.

cacheSolve <-function(x, ...){
		m<-x$getSolve()
		if(!is.null(m)){
			message("getting cached data")
			return(m)
		}
		data<-x$get()
		m<-solve(data, ...)
		x$setSolve(m)
		m			
}

## To test the pair of the functions,
## Un-comment the following commands.
#source("cachematrix.R")
#c<-rbind(c(1, -1/4), c(-1/4, 1))
#supermatrix<-makeCacheMatrix(c)
#cacheSolve(supermatrix)



