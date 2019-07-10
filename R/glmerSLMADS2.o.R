#' 
#' @title glmeSLMADS2.o
#' @description This is the second serverside function called by ds.glmSLMA.o.
#' @details It is a function
#' @import lme4
#' @export
glmerSLMADS2.o <- function(formula, family, offset, weights, dataName){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  #thr <- listDisclosureSettingsDS.o()
  #nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset<-as.numeric(thr$nfilter.subset)
  #nfilter.string<-as.numeric(thr$nfilter.string)
  #############################################################
  
  errorMessage2 <- "No errors"
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
  formulatext <- gsub("(1|", "", formulatext, fixed=TRUE)
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
  
  ##################################################################

  #mg <- glm(formula2use, family=family, offset=offset, weights=weights, data=dataDF)
  mg <- glmer(formula2use, family=family, offset=offset, weights=weights, data=dataDF)
# 
#   outlist<-list(rank=mg$rank, aic=mg$aic, 
#                 iter=mg$iter, converged=mg$converged,
#                 boundary=mg$boundary, na.action=options("na.action"), call=summary(mg)$call, terms=summary(mg)$terms,
#                 contrasts=summary(mg)$contrasts, aliased=summary(mg)$aliased, dispersion=summary(mg)$dispersion,
#                 data=dataName, df=summary(mg)$df, Ntotal=Ntotal, Nvalid=Nvalid, Nmissing=Nmissing,
#                 cov.unscaled=summary(mg)$cov.unscaled, cov.scaled=summary(mg)$cov.scaled,
#                 offset=varname.offset, weights=varname.weights,VarCovMatrix=NA,CorrMatrix=NA,
#                 deviance.null=mg$null.deviance, df.null=mg$df.null, deviance.resid=mg$deviance, df.resid=mg$df.residual,
#                 formula=mg$formula, family=mg$family,coefficients=summary(mg)$coefficients)

  outlist = summary(mg)
  
  return(outlist)
  
}
# AGGREGATE FUNCTION
# glmerSLMADS2.o
