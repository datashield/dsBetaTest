#' 
#' @title Generates matrices for the sum of squares
#' @description This function creates matrices having elements the sum of
#' squares that will be used on random intercept linear model method.
#' @details This function returns a list with four elements. The first is the
#' length of a variable's vector when we remove from the data frame any rows
#' with NAs.   
#' @param ..., any explanatory variables
#' @param yvar, the outcome variable
#' @return (i) the number of complete cases, (ii) the sum of squares of 
#' explanatory variables, (iii) the sum of products of each explanatory
#' variable with the outcome variable, and (iv) the sum of squares of the
#' outcome variable 
#' @author Avraam D; Jones EM; van den Heuvel ER
#' @export
#' 
rilmDS.b <- function (..., yvar){

  # create a data frame for the variables and remove any rows that include NAs
  data.table <- cbind.data.frame(...)
  data.complete <- stats::complete.cases(data.table)
  data.table.no.miss <- data.table[data.complete,]

  # Calculate the length "N.sub" of a variable vector after removing NAs and the number of covariates "N.xvar" 	
  N.sub <- dim(data.table.no.miss)[1]
  N.xvar <- dim(data.table.no.miss)[2]-1

  # create a vector of ones with the same length as a "clean" variable
  ones <- rep(1, N.sub)

  # create a new data frame where the first column is a vector of ones and the next columns are the covariates and the dependend variables
  datamatrix <- cbind.data.frame(ones, data.table.no.miss)

  XX <- matrix(ncol=(N.xvar+1), nrow=(N.xvar+1))
  XY <- matrix(ncol=1, nrow=(N.xvar+1))
  YY <- matrix(ncol=1, nrow=1)
  
  for(m in 1:(N.xvar+1)){
    for(p in 1:(N.xvar+1)){
      XX[m,p] <- sum(as.numeric(as.character(datamatrix[,m]))*as.numeric(as.character(datamatrix[,p])))
    }
  }
  
  for(m in 1:(N.xvar+1)){
    XY[m] <- sum(as.numeric(as.character(datamatrix[,m]))*as.numeric(as.character(datamatrix[,N.xvar+2])))
  }

  YY <- sum(as.numeric(as.character(datamatrix[,N.xvar+2]))*as.numeric(as.character(datamatrix[,N.xvar+2])))

  return(list(N.sub, XX, XY, YY))	

}
#AGGREGATE FUNCTION
# rilmDS.b
