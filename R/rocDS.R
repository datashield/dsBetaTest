#'
#' @title Computes the important points of a roc curve for given reference/prediction
#' @description Calculates roc points.
#' @details if the length of prediction and reference vectors don't match an error is returned.
#' @param prediction a numeric vector (values between 0 and 1) holding the tendency to lean towards a positive or negative outcome.
#' @param reference a logical (boolean) vector refering to the "truth" on which case is positive/true or negative/false.
#' @return two same-length numerical vectors corresponding to the sensitivity/specificity values to draw a roc curve.
#' @author Matthis A
#' @export
#'
rocDS <- function(reference, prediction){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  # thr <- listDisclosureSettingsDS()
  # nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  res <- pROC::roc(reference, prediction)

  out.specificities <- res$specificities
  out.sensitivities <- res$sensitivities

  nex <- Position(function(x) x>0.5, res$thresholds)
  pre <- nex - 1
  s <- (res$thresholds[nex]-0.5)/(res$thresholds[nex]-res$thresholds[pre])
  out.tspec <- s*res$specificities[nex]+(1-s)*res$specificities[pre]
  out.tsens <- s*res$sensitivities[nex]+(1-s)*res$sensitivities[pre]

  out.obj <- list(Specificities=out.specificities,Sensitivities=out.sensitivities, Threshold=c(x=out.tspec, y=out.tsens))
  return(out.obj)

}
#AGGREGATE FUNCTION
# rocDS
