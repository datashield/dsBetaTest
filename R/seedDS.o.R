#' 
#' @title Generates a seed number
#' @description This function generates the seed number based on the input vector.
#' @details The function aims to define a fixed random number generator based on the input vector
#' that will be used for the generation of normally distributed noise that is used mainly in the 
#' probabilistic approach of generating non-disclosive graphs in DataSHIELD (see for example the
#' ds.scatterPlot function). The recurrent use of a probabilistic anonymisation technique on a 
#' given dataset can reveal inference of the true records if the random number that generates the
#' embedded noise is different each time it is run. Therefore, to block potential inferential
#' disclosure from a malicious attacker, we fix the random number generator in a value that is
#' specified by the input variable. To find this number we first remove from the input variable any
#' missing and zero values. Then we take all unique values and check that their length is more 
#' than a specific percentage of the length of the initial vector. This percentage is specified
#' by the protection filter 'nfilter.levels' set in Opals. If the length of the unique values after
#' suppression of missing and zeros is less than the specified percentage of the initial length,
#' then the function does not generate the seed number and returns an error message to the client.
#' If the length of the unique values is bigger than the required length then the function
#' multiplies each element of the vector of unique values by 0.1234 (arbitrary choice), to prevent
#' the case of having only integer values and then calculates the logaritms to prevent the case of
#' having very small numbers ($x_i<<1$). Then the function finds the 10%, 17%, 24%, 31%, 38%, 45%,
#' 52%, 59%, 66%, 73%, 80% and 87% quantiles and estimates their module if divided by 0.25. 
#' Finally, it takes the first three decimals of each value and their sum is the seed number which
#' is assigned at each study.
#' @param x the name of a numeric vector, the x-variable.
#' @return the seed number is assigned to the serverside.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
seedDS.o <- function(x=NULL){
 
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    #
  thr <- listDisclosureSettingsDS.o()                         #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                 #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)             #
  nfilter.levels <- as.numeric(thr$nfilter.levels)            #
  #############################################################
  
 # remove any missing  
 x.noMissing <- as.vector(stats::na.omit(x))
 
 # remove any zeros to prevent the case that many values of x are zeros
 if (length(which(x.noMissing==0))>=1){
   x.noZeros <- x.noMissing[-which(x.noMissing==0)]
 }else{
   x.noZeros <- x.noMissing
 }
 
 # take the unique values to check that there are enough distinct observations 
 x.unique <- unique(x.noZeros)

 # multiply all values with a continuous number (it is set arbitrarily to 0.1234) to prevent the case that all values of x are integers
 # and convert to their logarithm to avoid very small numbers (x << 1)
 x.new <- log(abs(x.unique * 0.1234))
 
 # Check if the new vector has more than 33% of the initial vector's observations
 if(length(x.new) < nfilter.levels*length(x)){
   stop(paste0("the vector that generates the seed number generator has less than ", nfilter.levels*100, "% of the initial vector's observations"), call.=FALSE)
 }else{
   # find the 10%, 17%, 24%, 31%, 38%, 45%, 52%, 59%, 66%, 73%, 80% and 87% quantiles
   x.quants <- stats::quantile(x.new, probs = seq(0.1, 0.9, 0.07), na.rm = FALSE, names = FALSE, type = 1)
   # find the module of each quantile divided by 0.25
   x.modulo <- x.quants %% 0.25
   # take the first three decimals of each modulo and then their sum is the seed number
   seed <- sum(floor(x.modulo*1000))
 }
  
 return(seed)
  
}
# ASSIGN FUNCTION
# seedDS.o
