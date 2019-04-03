#' 
#' @title tTestFDS2
#' @description This is the second serverside function called by ds.tTestF. This is a modified version of the ds.glm function
#' coded specifically to undertake an unpaired t-test analysis specified using a glm formula.
#' @details For more details please see the extensive headers for ds.tTestF and ds.glm.o
#' @param formula a clientside provided character string specifying a glm() formula consistent with R syntax which specifies a t-test
#' @param family a glm() family consistent with R syntax and a t-test: must be "gaussian"
#' @param beta.vect a numeric vector created by the clientside function specifying the
#' vector of regression coefficients at the current iteration
#' @param offset an optional variable providing a regression offset
#' @param weights an optional variable providing regression weights
#' @param data an optional character string specifying a data.frame object holding
#' the data to be analysed under the specified model same 
#' @author Burton PR;Gaye,A;Laflamme,P
#' @export
tTestFDS2 <- function (formula, family, beta.vect, offset, weights, data) {
  
  # Get the value of the 'data' parameter provided as character on the client side
  # Same is done for offset and weights lower down function
  
  if(is.null(data)){
    dataTable <- NULL 
  }else{
    dataTable <- eval(parse(text=data), envir = parent.frame())
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
      assign(elt[length(elt)], eval(parse(text=model.variables[i]), envir = parent.frame()), envir = parent.frame())
      originalFormula <- gsub(model.variables[i], elt[length(elt)], originalFormula, fixed=TRUE)
      varnames <- append(varnames, elt[length(elt)])
    }else{
      varnames <- append(varnames, elt)
    }
  }
  varnames <- unique(varnames)
  
  ######################## 
  #ADD
  #Identify and use variable names to count missings
  cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
  all.data <- eval(parse(text=cbindraw.text), envir = parent.frame())
  
  Ntotal<-dim(all.data)[1]
  
  nomiss.any<-complete.cases(all.data)
  nomiss.any.data<-all.data[nomiss.any,]
  N.nomiss.any<-dim(nomiss.any.data)[1]
  
  Nvalid<-N.nomiss.any
  Nmissing<-Ntotal-Nvalid
  
  nomiss.qvar<-complete.cases(all.data[,1])
  nomiss.qvar.data<-all.data[nomiss.qvar,]
  N.nomiss.qvar<-dim(nomiss.qvar.data)[1]
  Nmissing.qvar<-Ntotal-N.nomiss.qvar
  
  nomiss.bvar<-complete.cases(all.data[,2])
  nomiss.bvar.data<-all.data[nomiss.bvar,]
  N.nomiss.bvar<-dim(nomiss.bvar.data)[1]
  Nmissing.bvar<-Ntotal-N.nomiss.bvar
  
  #######################################
  # Now fit model specified in formula: by using x=TRUE this is how we generate all of the model terms
  # and the data that underlie them. This will include a vector of 1s for the intercept and
  # any dummy variables required for factors
  
  formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula))), env = parent.frame()) # here we need the formula as a 'call' object
  mod.glm.ds <- glm(formula2use, family=family, x=TRUE, control=glm.control(maxit=1), contrasts=NULL, data=dataTable)
  
  X.mat.orig <- as.matrix(mod.glm.ds$x)
  y.vect.orig <-as.vector(mod.glm.ds$y)
  f<-mod.glm.ds$family
  
  formulatest<-0
  if(min(X.mat.orig[,1]!=1||max(X.mat.orig[,1]!=1))){
    formulatest<-1
  }
  
  # Remove rows of offset or weights which contain NA in any Y or X variable
  # Rows where offset or weights are missing but Y and X are non-NA, remain at this stage
  cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ")")
  dtemp <- eval(parse(text=cbindtext), envir = parent.frame())
  # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
  row.noNA.YX<-complete.cases(dtemp)
  
  #Both weights and offset
  if(!(is.null(weights))&&!(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", weights, ",", offset,")")
    dtemp <- eval(parse(text=cbindtext), envir = parent.frame())
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[row.noNA.YX,]
    offsetvar.orig <- cmplt[, dim(cmplt)[2]] 
    weightsvar.orig <- cmplt[, (dim(cmplt)[2]-1)]    
  }
  
  #Offset no weights 
  if(is.null(weights)&&!(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", offset, ")")
    dtemp <- eval(parse(text=cbindtext), envir = parent.frame())
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[row.noNA.YX,]
    offsetvar.orig <- cmplt[, dim(cmplt)[2]]
  }
  
  #Weights no offset
  if(!(is.null(weights))&&(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", weights, ")")
    dtemp <- eval(parse(text=cbindtext), envir = parent.frame())
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
  
  return(list(family=f, info.matrix=info.matrix, score.vect=score.vect, numsubs=numsubs, dev=dev, formulatest=formulatest,
              Nvalid=Nvalid,Nmissing=Nmissing,Ntotal=Ntotal,Nmissing.qvar=Nmissing.qvar,Nmissing.bvar=Nmissing.bvar))
  
}
#AGGREGATE FUNCTION
#tTestFDS2