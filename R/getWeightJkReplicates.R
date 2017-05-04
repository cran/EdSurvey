#' @title Return the jackknife replicate weights associated with a weight variable.
#'
#' @description Return the jackknife replicate weights on an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} associated with a weight variable.
#'
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#' @param var character indicating the name of the variable for which the
#'            jackknife replicate weights are desired.
#'
#' @return A character vector of the jackknife replicate weights.
#'
#' @example \man\examples\getWeightJkReplicates.R
#' @author Michael Lee and Paul Bailey
#' @export
getWeightJkReplicates <- function(var, data) {
  sdf <- data
  if (!inherits(sdf, c("edsurvey.data.frame", "light.edsurvey.data.frame"))) 
    stop(paste0("The argument ", sQuote("sdf"), " must be an edsurvey.data.frame or a light.edsurvey.data.frame. See the 'Using the ", 
      dQuote("EdSurvey"), " Package's getData Function to Manipulate the NAEP Primer Data vignette' for how to work with data in a light.edsurvey.data.frame."))
  stopifnot(isWeight(var, sdf))
  if (inherits(sdf, c("edsurvey.data.frame"))) {
    # sdf is an edsurvey.data.frame, so jkbase and jkreplicates returned in sdf
    return(paste0(sdf$weights[[var]]$jkbase, sdf$weights[[var]]$jksuffixes))
  } else {
    # sdf is a light.edsurvey.data.frame, so jkbase and jkreplicates returned in
    # attributes(sdf)
    return(paste0(attributes(sdf)$weights[[var]]$jkbase, attributes(sdf)$weights[[var]]$jksuffixes))
  }
}