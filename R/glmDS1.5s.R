#' @title
#' glmDS1.5s
#'
#' @description
#' TODO
#'
#' @export
#'

glmDS1.5s <- function (formula, family, weights, data) {
  
  # get the value of the 'data' and 'weights' parameters provided as character on the client side
  if(is.null(data)){
    dataTable <- NULL 
  }else{
    dataTable <- eval(parse(text=data))
  }
   
   formulatext <- Reduce(paste, deparse(formula))
   originalFormula <- formulatext
  
# Convert formula string into separate variable names split by |
  formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)

   formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object
   mod.glm.ds <- glm(formula2use, family=family, x=TRUE, control=glm.control(maxit=1), contrasts=NULL, data=dataTable)
 
  
#Remember model.variables and then varnames INCLUDE BOTH yvect AND linear predictor components 
	model.variables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
 
	 varnames <- c()
	  for(i in 1:length(model.variables)){
	    elt <- unlist(strsplit(model.variables[i], split="$", fixed=TRUE))
	    if(length(elt) > 1){
	      assign(elt[length(elt)], eval(parse(text=model.variables[i])))
	      originalFormula <- gsub(model.variables[i], elt[length(elt)], originalFormula, fixed=TRUE)
	      varnames <- append(varnames, elt[length(elt)])
	    }else{
	      varnames <- append(varnames, elt)
	    }
	  }

	varnames <- unique(varnames) 

   X.mat <- as.matrix(mod.glm.ds$x)
  
   dimX<-dim((X.mat))
 
   y.vect<-as.vector(mod.glm.ds$y)

   coef.names<-names(mod.glm.ds$coefficients)

   if(is.null(weights)){
	w.vect<-rep(1,length(y.vect))
	}else{
	ftext <- paste0("cbind(",weights,")")
	w.vect <- eval(parse(text=ftext))
	}


#SET FILTER THRESHOLD
#NEEDS SETTING FROM INTERNAL OPAL FILTER
filter.threshold<-2



#CHECK Y VECTOR VALIDITY
	y.invalid<-0

	unique.values.y<-unique(y.vect)
	unique.values.noNA.y<-unique.values.y[complete.cases(unique.values.y)]

	if(length(unique.values.noNA.y)==2){
		tabvar<-table(y.vect)[table(y.vect)>=1]
		min.category<-min(tabvar)
		if(min.category<=filter.threshold)y.invalid<-1
		}


#CHECK X MATRIX VALIDITY 
#Check no dichotomous X vectors with between 1 and filter.threshold 
#observations at either level 
  	num.Xpar<-dimX[2]

	Xpar.invalid<-rep(0,num.Xpar)

  	for(pj in 1:num.Xpar){
	unique.values<-unique(X.mat[,pj])
	unique.values.noNA<-unique.values[complete.cases(unique.values)]

	if(length(unique.values.noNA)==2){
		tabvar<-table(X.mat[,pj])[table(X.mat[,pj])>=1]
		min.category<-min(tabvar)
		if(min.category<=filter.threshold)Xpar.invalid[pj]<-1
		}
	}

#Check model not excessively parameterised don't allow models to fit
#if number of model parameters >= 50% of sample size in a given study

	num.observations<-dimX[1]
	if((num.Xpar/num.observations)>=0.5){
		Xpar.invalid<-rep(1,num.Xpar)
	}

#CHECK W VECTOR VALIDITY
	w.invalid<-0

	unique.values.w<-unique(w.vect)
	unique.values.noNA.w<-unique.values.w[complete.cases(unique.values.w)]

	if(length(unique.values.noNA.w)==2){
		tabvar<-table(w.vect)[table(w.vect)>=1]
		min.category<-min(tabvar)
		if(min.category<=filter.threshold)w.invalid<-1
		}

#If y, X or w data invalid, the return of these y.invalid, Xpar.invalid and w.invalid
#values to ds.glm.5s will lead to a warning and a controlled shut down of the function.
#But in case a user modifies the client side function to circumvent the trap, the equivalent
#tests in glmDS2.5s destroy the info.matrix and score.vector in the affected study so
#the model simply cannot fit 

return(list(dimX=dimX,coef.names=coef.names,y.invalid=y.invalid,Xpar.invalid=Xpar.invalid,w.invalid=w.invalid))
  
  
}
#glmDS1.5s

