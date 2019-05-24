#' 
#' @title listDisclosureSettingsDS
#' @description This serverside function is an aggregate function that is called by the
#' ds.listDisclosureSettings
#' @details For more details see the extensive header for ds.listDisclosureSettings
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#'
listDisclosureSettingsDS.o <- function(){

  nf.tab <- getOption("nfilter.tab")
  if (nf.tab == NULL)
    nf.tab <- getOption("default.nfilter.tab")
  nf.subset <- getOption("nfilter.subset")
  if (nf.subset == NULL)
    nf.subset <- getOption("default.nfilter.subset")
  nf.glm <- getOption("nfilter.glm")
  if (nf.glm == NULL)
    nf.glm <- getOption("default.nfilter.glm")
  nf.string <- getOption("nfilter.string")
  if (nf.string == NULL)
    nf.string <- getOption("default.nfilter.string")
  nf.stringShort <- getOption("nfilter.stringShort")
  if (nf.stringShort == NULL)
    nf.stringShort <- getOption("default.nfilter.stringShort")
  nf.kNN <- getOption("nfilter.kNN")
  if (nf.kNN == NULL)
    nf.kNN <- getOption("default.nfilter.kNN")
  nf.levels <- getOption("nfilter.levels")
  if (nf.levels == NULL)
    nf.levels <- getOption("default.nfilter.levels")
  nf.noise <- getOption("nfilter.noise")
  if (nf.noice == NULL)
    nf.noise <- getOption("default.nfilter.noise")
  nfilter.privacy.old <- getOption("datashield.privacyLevel")

  return(list(nfilter.tab=nf.tab,nfilter.subset=nf.subset,nfilter.glm=nf.glm,nfilter.string=nf.string,
              nfilter.stringShort=nf.stringShort,nfilter.kNN=nf.kNN,nfilter.levels=nf.levels,
              nfilter.noise=nf.noise,nfilter.privacy.old=nfilter.privacy.old))
}
#AGGREGATE FUNCTION
# listDisclosureSettingsDS.o
