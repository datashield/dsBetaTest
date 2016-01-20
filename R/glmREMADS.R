#' @title
#' glmREMADS
#'
#' @description
#' TODO
#'
#' @export
#'

glmREMADS <- function (formula, family, beta.vect, offset, weights, data) {

 
# Get the value of the 'data' parameter provided as character on the client side
# Same is done for offset and weights lower down function

  if(is.null(data)){
    dataTable <- NULL 
  }else{
    dataTable <- eval(parse(text=data))
  }

 
# Rewrite formula extracting variables nested in strutures like data frame or list
# (e.g. D$A~D$B will be re-written A~B)
# Note final product is a list of the variables in the model (yvector and covariates)
# it is NOT a list of model terms - these are derived later

# Convert formula into an editable character string
  formulatext <- Reduce(paste, deparse(formula))

# First save original model formala
  originalFormula <- formulatext

# Convert formula string into separate variable names split by |
  formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)

  
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

######################## 
#ADD TO ds.glm in DS4.0 - enumerates and reports missing data

#Identify and use variable names to count missings
	cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
	all.data <- eval(parse(text=cbindraw.text))

	Ntotal<-dim(all.data)[1]
	
	nomiss.any<-complete.cases(all.data)
	nomiss.any.data<-all.data[nomiss.any,]
	N.nomiss.any<-dim(nomiss.any.data)[1]

	Nvalid<-N.nomiss.any
	Nmissing<-Ntotal-Nvalid


#######################################
  
# Now fit model specified in formula: by using x=TRUE this is how we generate all of the model terms
# and the data that underlie them. This will include a vector of 1s for the intercept and
# any dummy variables required for factors
 
    formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object
    mod.glm.ds <- glm(formula2use, family=family, x=TRUE, control=glm.control(maxit=1), contrasts=NULL, data=dataTable)

 X.mat.orig <- as.matrix(mod.glm.ds$x)
 y.vect.orig <-as.vector(mod.glm.ds$y)
 f<-mod.glm.ds$family

# Remove rows of offset or weights which contain NA in any Y or X variable
# Rows where offset or weights are missing but Y and X are non-NA, remain at this stage
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ")")
    dtemp <- eval(parse(text=cbindtext))
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    row.noNA.YX<-complete.cases(dtemp)

#Both weights and offset
if(!(is.null(weights))&&!(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", weights, ",", offset,")")
    dtemp <- eval(parse(text=cbindtext))
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[row.noNA.YX,]
    offsetvar.orig <- cmplt[, dim(cmplt)[2]] 
    weightsvar.orig <- cmplt[, (dim(cmplt)[2]-1)]    
  }
 
#Offset no weights 
if(is.null(weights)&&!(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", offset, ")")
    dtemp <- eval(parse(text=cbindtext))
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[row.noNA.YX,]
    offsetvar.orig <- cmplt[, dim(cmplt)[2]]
  }
  
#Weights no offset
if(!(is.null(weights))&&(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", weights, ")")
    dtemp <- eval(parse(text=cbindtext))
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[row.noNA.YX,]
    weightsvar.orig <- cmplt[, dim(cmplt)[2]]    
  } 
 
#Now work with y vector and X matrix from actual model (with all terms explicit)

#Strip rows of y, X matrix, offset and weights if missing values in offset or weights
#If an offset is not specified then NAs in it are meaningless and so have no impact
#If weights are not specified then NAs in it are meaningless and so have no impact
 
#Both weights and offset
 if(!(is.null(weights))&&!(is.null(offset))){
 YXWO.orig<-cbind(y.vect.orig,X.mat.orig,weightsvar.orig,offsetvar.orig)
 YXWO.complete<-YXWO.orig[complete.cases(YXWO.orig),]
 numcol.YXWO<-dim(YXWO.orig)[2]
 y.vect<-YXWO.complete[,1]
#NB - must specify X.mat as.matrix because otherwise with a one parameter linear predictor
#ie just the column of 1s for the intercept, X.mat is n x 1 and defaults to vector which does
#not then work in the matrix multiplication code below 
 X.mat<-as.matrix(YXWO.complete[,(2:(numcol.YXWO-2))])
 weightsvar<-YXWO.complete[,numcol.YXWO-1]
 offsetvar<-YXWO.complete[,numcol.YXWO]
 }

#Offset no weights
 if(is.null(weights)&&!(is.null(offset))){
 YXO.orig<-cbind(y.vect.orig,X.mat.orig,offsetvar.orig)
 YXO.complete<-YXO.orig[complete.cases(YXO.orig),]
 numcol.YXO<-dim(YXO.orig)[2]
 y.vect<-YXO.complete[,1]
#NB - must specify X.mat as.matrix because otherwise with a one parameter linear predictor
#ie just the column of 1s for the intercept, X.mat is n x 1 and defaults to vector which does
#not then work in the matrix multiplication code below 
 X.mat<-as.matrix(YXO.complete[,(2:(numcol.YXO-1))])
 weightsvar<-rep(1,length(y.vect))
 offsetvar<-YXO.complete[,numcol.YXO]
 }
 
#Weights no offset
 if(!(is.null(weights))&&(is.null(offset))){
 YXW.orig<-cbind(y.vect.orig,X.mat.orig,weightsvar.orig)
 YXW.complete<-YXW.orig[complete.cases(YXW.orig),]
 numcol.YXW<-dim(YXW.orig)[2]
 y.vect<-YXW.complete[,1]
 X.mat<-as.matrix(YXW.complete[,(2:(numcol.YXW-1))])
 weightsvar<-YXW.complete[,numcol.YXW]
 offsetvar<-rep(0,length(y.vect))
 }
 
 #No weights or offset
 if(is.null(weights)&&(is.null(offset))){
 y.vect<-y.vect.orig
 X.mat<-X.mat.orig
 weightsvar<-rep(1,length(y.vect))
 offsetvar<-rep(0,length(y.vect))
 }
 
 
numsubs<-length(y.vect)

#Convert beta.vect from transmittable (character) format to numeric 

  beta.vect.n <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  beta.update.vect<-rep(0,length(beta.vect.n))

  iteration.count<-0

  #Convergence state needs to be monitored.
  converge.state<-FALSE
  
  #Define a convergence criterion. This value of epsilon corresponds to that used
  #by default for GLMs in R (see section S3 for details)
  epsilon<-1.0e-08

  #Provide arbitrary starting value for deviance to enable subsequent calculation of the
  #change in deviance between iterations
  dev.old<-9.99e+99
 



#############################START INTERNAL STUDY ITERATIVE LOOP 

#NEED TO RESTATE maxit AT PRESENT HARD SET AT 25
 
 while(!converge.state && iteration.count < 25) {#start while loop
    
    iteration.count<-iteration.count+1
    beta.vect.n<-beta.vect.n+beta.update.vect


    
  
#If an offset is specified, add it directly to the values in the linear predictor
  if(!is.null(offset)){
    lp.vect <- (X.mat%*%beta.vect.n)+offsetvar
  }else{
    lp.vect <- (X.mat%*%beta.vect.n)   
  }
   
#Use the available functions for family f to generate the components giving the deviance and
#the working weights for the IRLS algorithm
 
  mu.vect<-f$linkinv(lp.vect)
  mu.eta.val<-f$mu.eta(lp.vect)
  var.vect<-f$variance(mu.vect)
  
#If a prior weights vector is specified multiply the working weights by the prior weights 
  if(!is.null(weights)){
  W.vect<-as.vector(mu.eta.val^2/var.vect)
  W.vect<-W.vect*weightsvar
  dev<-sum(f$dev.resids(y.vect, mu.vect, rep(1, length(y.vect)))*weightsvar)
  }else{
  W.vect<-as.vector(mu.eta.val^2/var.vect)
  dev<-sum(f$dev.resids(y.vect, mu.vect, rep(1, length(y.vect))))
  }

#Generate information matrix as XWX  
  WX.mat<-W.vect*X.mat
  info.matrix<-t(X.mat)%*%WX.mat
  
#Generate score vector as XWz (where z is working response vector on scale of linear predictor)
#See theoretical basis in the .pdf in RELEVANT.GLM.THEORY directory.
#Note mu.et.val is first differential of inverse link function (d.mu by d.eta)
#which is inverse of first diff of link function (g') in thoretical explanation

  u.vect<-(y.vect-mu.vect)*1/mu.eta.val
  W.u.mat<-matrix(W.vect*u.vect)
  score.vect<-t(X.mat)%*%W.u.mat

##########################
#BACKUP DISCLOSURE TRAP
#If y, X or w data are invalid but user has modified clientside
#function (ds.glm) to circumvent trap, model will get to this point without
#giving a controlled shut down with a warning about invalid data.
#So as a safety measure, we will now use the same test that is used to
#trigger a controlled trap in the clientside function to destroy the
#score.vector and information.matrix in the study with the problem.
#So this will make model fail without explanation

#Disclosure code from glmDS1.5s

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
	dimX<-dim((X.mat))

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
#Check model not excessively parameterised. Don't allow models to fit
#if number of model parameters >= 50% of sample size in a given study

	num.observations<-dimX[1]
	if((num.Xpar/num.observations)>=0.5){
		Xpar.invalid<-rep(1,num.Xpar)
	}

#CHECK W VECTOR VALIDITY
	w.invalid<-0

#Keep same object name as in glmDS1.5s
	w.vect<-weightsvar

	unique.values.w<-unique(w.vect)
	unique.values.noNA.w<-unique.values.w[complete.cases(unique.values.w)]

	if(length(unique.values.noNA.w)==2){
		tabvar<-table(w.vect)[table(w.vect)>=1]
		min.category<-min(tabvar)
		if(min.category<=filter.threshold)w.invalid<-1
		}

#########################
disclosure.risk<-0


#If there is a disclosure risk destroy the info.matrix and score.vect
if(y.invalid>0||w.invalid>0||sum(Xpar.invalid)>0){
	info.matrix<-NA
	score.vector<-NA
	disclosure.risk<-1		
	}
##########################


    #Calculate value of convergence statistic and test whether meets convergence criterion
    converge.value<-abs(dev-dev.old)/(abs(dev)+0.1)
    if(converge.value<=epsilon)converge.state<-TRUE
    if(converge.value>epsilon)dev.old<-dev
    

    #Create variance covariance matrix as inverse of information matrix
    variance.covariance.matrix<-solve(info.matrix)

    # Create beta vector update terms
    beta.update.vect<-variance.covariance.matrix %*% score.vect

    }#stop while loop




#####START OF TEXT FROM CLIENT FUNCTION
  

#INTEGRATE RETURNED OUTPUT
    if(disclosure.risk>0){
	message("DISCLOSURE RISK IN y.vect, X.mat or w.vect")
	output.blocked.information.1<-"POTENTIAL DISCLOSURE RISK"
	output.blocked.information.2<-"SO SCORE VECTOR AND INFORMATION MATRIX DESTROYED"

   	return(list(output.blocked.information.1,
			output.blocked.information.2))
	}

  
    family.identified<-0
    
    beta.vect.final<-beta.vect.n
    
    scale.par <- 1
    if(f$family== 'gaussian') {
      scale.par <- dev / (numsubs-length(beta.vect.final))
    }
	
 

    family.identified<-1
    se.vect.final <- sqrt(diag(variance.covariance.matrix)) * sqrt(scale.par)
    z.vect.final<-beta.vect.final/se.vect.final
    pval.vect.final<-2*pnorm(-abs(z.vect.final))
    parameter.names<-names(score.vect[,1])
    model.parameters<-cbind(beta.vect.final,se.vect.final,z.vect.final,pval.vect.final)
    dimnames(model.parameters)<-list(parameter.names,c("Estimate","Std. Error","z-value","p-value"))

	

 
###NEED TO ADD CI flag to serverside call
  
 #   if(CI > 0)
  CI<-0.95
      ci.mult <- qnorm(1-(1-CI)/2)
      low.ci.lp <- model.parameters[,1]-ci.mult*model.parameters[,2]
      hi.ci.lp <- model.parameters[,1]+ci.mult*model.parameters[,2]
      estimate.lp <- model.parameters[,1]
      
      
      
      if(family=="gaussian"){
        estimate.natural <- estimate.lp
        low.ci.natural <- low.ci.lp
        hi.ci.natural <- hi.ci.lp
        name1 <- paste0("low",CI,"CI")
        name2 <- paste0("high",CI,"CI")
        ci.mat <- cbind(low.ci.lp,hi.ci.lp)
        dimnames(ci.mat) <- list(NULL,c(name1,name2))   
      }
      
      if(family=="binomial"){
        family.identified  <-  1
        num.parms <- length(low.ci.lp)
          name1 <- paste0("low",CI,"CI.LP")
          name2 <- paste0("high",CI,"CI.LP")
          name3 <- paste0("P_OR")
          name4 <- paste0("low",CI,"CI.P_OR")
          name5 <- paste0("high",CI,"CI.P_OR")
         estimate.natural <- exp(estimate.lp)/(1+exp(estimate.lp))
        low.ci.natural <- exp(low.ci.lp)/(1+exp(low.ci.lp))
        hi.ci.natural <- exp(hi.ci.lp)/(1+exp(hi.ci.lp))
        if(num.parms > 1){
          estimate.natural[2:num.parms] <- exp(estimate.lp[2:num.parms])
          low.ci.natural[2:num.parms] <- exp(low.ci.lp[2:num.parms])
          hi.ci.natural[2:num.parms] <- exp(hi.ci.lp[2:num.parms])
       }       
        ci.mat <- cbind(low.ci.lp,hi.ci.lp,estimate.natural,low.ci.natural,hi.ci.natural)
        dimnames(ci.mat) <- list(NULL,c(name1,name2,name3,name4,name5))
        
      }
      
      if(family=="poisson"){
        family.identified <- 1
        num.parms <- length(low.ci.lp)
        estimate.natural <- exp(estimate.lp)
        low.ci.natural <- exp(low.ci.lp)
        hi.ci.natural <- exp(hi.ci.lp)
        name1 <- paste0("low",CI,"CI.LP")
        name2 <- paste0("high",CI,"CI.LP")
        name3 <- paste0("EXPONENTIATED RR")
        name4 <- paste0("low",CI,"CI.EXP")
        name5 <- paste0("high",CI,"CI.EXP")
        ci.mat <- cbind(low.ci.lp,hi.ci.lp,estimate.natural,low.ci.natural,hi.ci.natural)
        dimnames(ci.mat) <- list(NULL,c(name1,name2,name3,name4,name5))        
      }
      
      if(family.identified==0)
      {
        estimate.natural <- estimate.lp
        low.ci.natural <- low.ci.lp
        hi.ci.natural <- hi.ci.lp
        name1 <- paste0("low",CI,"CI")
        name2 <- paste0("high",CI,"CI")
        ci.mat <- cbind(low.ci.lp,hi.ci.lp)
        dimnames(ci.mat) <- list(NULL,c(name1,name2))   
      }
      
    

   

    model.parameters<-cbind(model.parameters,ci.mat)
 
#HARD WIRING IN FORMULA TEXT NEED TO ADD offset and weights LATER
       formulatext <- paste0(Reduce(paste, deparse(formula)))

   
#    if(!is.null(offset)&&!is.null(weights)){
#       formulatext <- paste0(Reduce(paste, deparse(formula)), paste0(" + offset(", offset, ")"), paste0(" + weights(", weights, ")"))
#       }
#    if(!is.null(offset)&&is.null(weights)){
#       formulatext <- paste0(Reduce(paste, deparse(formula)), paste0(" + offset(", offset, ")"))
#       }
#    if(is.null(offset)&&!is.null(weights)){
#       formulatext <- paste0(Reduce(paste, deparse(formula)), paste0(" + weights(", weights, ")"))
#       }
#
#    if(is.null(offset)&&is.null(weights)){
#       formulatext <- Reduce(paste, deparse(formula))
#       }
    
    
 
  


#################################END OF TEXT FROM CLIENT FUNCTION

  return(list(family=f, formulatext=formulatext, model.parameters=model.parameters,info.matrix=info.matrix, score.vect=score.vect, numsubs=numsubs, dev=dev,
				Nvalid=Nvalid,Nmissing=Nmissing,Ntotal=Ntotal,disclosure.risk=disclosure.risk,
                        iteration.count=iteration.count))  
}
#glmREMADS

