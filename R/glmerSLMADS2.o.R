#' 
#' @title glmeSLMADS2.o
#' @description This is the second serverside function called by ds.glmSLMA.o.
#' @details It is a function
#' @import lme4
#' @export
glmerSLMADS2.o <- function(formula, family, offset, weights, dataName, nAGQ=1){
  
  errorMessage <- "No errors"
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS.o()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################
  
  # Get the value of the 'data' parameter provided as character on the client side
  # Same is done for offset and weights lower down function
  
  if(!is.null(dataName)){
    dataDF <- eval(parse(text=dataName), envir = parent.frame())
  }else{
    dataDF <- NULL
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
  formulatext <- gsub("(", "", formulatext, fixed=TRUE)
  formulatext <- gsub("1", "", formulatext, fixed=TRUE)
  formulatext <- gsub("0", "", formulatext, fixed=TRUE)
  formulatext <- gsub(")", "", formulatext, fixed=TRUE)
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)
  
  
  # Remember model.variables and then varnames INCLUDE BOTH yvect AND linear predictor components 
  model.variables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
  
  varnames <- c()
  for(i in 1:length(model.variables)){
    elt <- unlist(strsplit(model.variables[i], split="$", fixed=TRUE))
    if(length(elt) > 1){
      assign(elt[length(elt)], eval(parse(text=model.variables[i]), envir = parent.frame()), envir = parent.frame())
      originalFormula.modified <- gsub(model.variables[i], elt[length(elt)], originalFormula, fixed=TRUE)
      varnames <- append(varnames, elt[length(elt)])
    }else{
      varnames <- append(varnames, elt)
    }
  }
  varnames <- unique(varnames)
  
  if(!is.null(dataName)){
    for(v in 1:length(varnames)){
      varnames[v]<-paste0(dataName,"$",varnames[v])
      test.string<-paste0(dataName,"$","1")
      if(varnames[v]==test.string)varnames[v]<-"1"
    }
    cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")		
  }else{
    cbindraw.text <- paste0("cbind(", paste(varnames, collapse=","), ")")
  }
  
  # Identify and use variable names to count missings
  all.data <- eval(parse(text=cbindraw.text), envir = parent.frame())
  
  Ntotal <- dim(all.data)[1]
  
  nomiss.any <- complete.cases(all.data)
  nomiss.any.data <- all.data[nomiss.any,]
  N.nomiss.any <- dim(nomiss.any.data)[1]
  
  Nvalid <- N.nomiss.any
  Nmissing <- Ntotal-Nvalid
  
  formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula))), env = parent.frame()) # here we need the formula as a 'call' object
  
  ################################################################## 
  #sort out offset and weights
  varname.offset <- paste0(offset)
  
  if(!(is.null(offset))){
    cbindtext.offset <- paste0("cbind(", offset,")")
    offset <- eval(parse(text=cbindtext.offset), envir = parent.frame())
  }
  else{
    assign(x = 'offset', value = NULL, envir = parent.frame())
  }
  
  varname.weights<-paste0(weights)
  
  if(!(is.null(weights))){
    cbindtext.weights <- paste0("cbind(", weights,")")
    weights <- eval(parse(text=cbindtext.weights), envir = parent.frame())
  }
  else{
    assign(x = 'weights', value = NULL, envir = parent.frame())
  }
  
  #### BEFORE going further we use the glm1 checks
  
  formulatext.glm = originalFormula
  
  # Convert formula string into formula string that will work for GLM
  formulatext.glm <- gsub(" ", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("(", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("1", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("0", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub(")", "", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("|", "+", formulatext.glm, fixed=TRUE)
  formulatext.glm <- gsub("++", "+", formulatext.glm, fixed=TRUE)
  
  formula2use.glm <- as.formula(paste0(Reduce(paste, deparse(formulatext.glm ))), env = parent.frame()) # here we need the formula as a 'call' object
  
  mod.glm.ds <- stats::glm(formula2use.glm, family=family, x=TRUE, control=stats::glm.control(maxit=1), contrasts=NULL, data=dataDF)
  
  X.mat <- as.matrix(mod.glm.ds$x)
  
  dimX <- dim((X.mat))
  
  y.vect <- as.vector(mod.glm.ds$y)
  
  ##############################################################
  #FIRST TYPE OF DISCLOSURE TRAP - TEST FOR OVERSATURATED MODEL#
  #TEST AGAINST nfilter.glm									  #
  ##############################################################
  
  glm.saturation.invalid <- 0
  num.p <- dimX[2]
  num.N <- dimX[1]
  
  if(num.p>nfilter.glm*num.N){
    glm.saturation.invalid <- 1
    errorMessage <- "ERROR: Model has too many parameters, there is a possible risk of disclosure - please simplify model"
    #DELETE return(errorMessage) 
  }
  
  coef.names <- names(mod.glm.ds$coefficients)
  
  if(is.null(weights)){
    w.vect <- rep(1,length(y.vect))
  }else{
    ftext <- paste0("cbind(",weights,")")
    w.vect <- eval(parse(text=ftext), envir = parent.frame())
  }
  
  ################################
  #SECOND TYPE OF DISCLOSURE TRAP#
  ################################
  
  #If y, X or w data are invalid but user has modified clientside
  #function (ds.glm) to circumvent trap, model will get to this point without
  #giving a controlled shut down with a warning about invalid data.
  #So as a safety measure, we will now use the same test that is used to
  #trigger a controlled trap in the clientside function to destroy the
  #score.vector and information.matrix in the study with the problem.
  
  #CHECK Y VECTOR VALIDITY
  y.invalid <- 0
  
  #COUNT NUMBER OF UNIQUE NON-MISSING VALUES - DISCLOSURE RISK ONLY ARISES WITH TWO LEVELS
  unique.values.noNA.y <- unique(y.vect[complete.cases(y.vect)])
  
  #IF TWO LEVELS, CHECK WHETHER EITHER LEVEL 0 < n < nfilter.tab
  
  if(length(unique.values.noNA.y)==2){
    tabvar<-table(y.vect)[table(y.vect)>=1]   #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
    min.category<-min(tabvar)
    if(min.category<nfilter.tab){
      y.invalid<-1
      errorMessage<-"ERROR: y vector is binary with one category less than filter threshold for table cell size"
    }
  }
  
  #CHECK X MATRIX VALIDITY 
  #Check no dichotomous X vectors with between 1 and filter.threshold 
  #observations at either level 
  dimX<-dim((X.mat))
  
  num.Xpar<-dimX[2]
  
  Xpar.invalid<-rep(0,num.Xpar)
  
  for(pj in 1:num.Xpar){
    unique.values.noNA<-unique((X.mat[,pj])[complete.cases(X.mat[,pj])]) 
    
    if(length(unique.values.noNA)==2){
      tabvar<-table(X.mat[,pj])[table(X.mat[,pj])>=1] #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
      min.category<-min(tabvar)
      if(min.category<nfilter.tab){
        Xpar.invalid[pj]<-1
        errorMessage<-"ERROR: at least one column in X matrix is binary with one category less than filter threshold for table cell size"
      }
    }
  }
  
  
  #CHECK W VECTOR VALIDITY
  w.invalid<-0
  
  unique.values.noNA.w<-unique(w.vect[complete.cases(w.vect)])
  
  if(length(unique.values.noNA.w)==2){
    tabvar<-table(w.vect)[table(w.vect)>=1]   #tabvar COUNTS N IN ALL CATEGORIES WITH AT LEAST ONE OBSERVATION
    min.category<-min(tabvar)
    if(min.category<nfilter.tab){
      w.invalid<-1
      errorMessage<-"ERROR: w vector is binary with one category less than filter threshold for table cell size"
    }
  }
  
  #Do the glmDS1 checks
  
  disclosure.risk<-0
  
  if(y.invalid>0||w.invalid>0||sum(Xpar.invalid)>0||glm.saturation.invalid>0){
    info.matrix<-NA
    score.vector<-NA
    disclosure.risk<-1
    mg<-NA
  }
  
  
  ##################################################################
  
  if(disclosure.risk==0){
    mg <- glmer(formula2use, offset=offset, weights=weights, family=family, data=dataDF, nAGQ = nAGQ)
    outlist = list(summary(mg))
    # outlist = list(call=summary(mg)$call, AICtab=summary(mg)$AICtab, REML=mg@devcomp$cmp[7], coefficients=summary(mg)$coefficients, RE=summary(mg)$varcor, data=dataName,
    #                Ntotal=Ntotal, Nvalid=Nvalid, Nmissing=Nmissing, cov.scaled=summary(mg)$vcov, ngrps = summary(mg)$ngrps,offset=varname.offset, weights=varname.weights,
    #                errorMessage = errorMessage, disclosure.risk = disclosure.risk)
  }
  else {
    outlist = list(errorMessage = errorMessage, disclosure.risk = disclosure.risk)
  }
  
  
  return(outlist)
}
# AGGREGATE FUNCTION
# glmerSLMADS2.o
