#' @title Return a value indicating whether a variable is a weight on an edsurvey.data.frame or light.edsurvey.data.frame.
#'
#' @description For an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}, return a value indicating whether a variable is a weight.
#'
#' @param sdf an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#' @param var character indicating the variable in question.
#'
#' @return A boolean (or vector when \code{var} is a vector) indicating if each element of \code{var}
#'         are weights.
#'
#' @details Note that this function returns TRUE only when \code{var} is the name of the weight used
#'         	for making estimates, not if the \code{var} is one of the individual jackknife replicates.
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\isWeight.R
#' @export
isWeight <- function(var, sdf) {
  if (!inherits(sdf, c("edsurvey.data.frame", "light.edsurvey.data.frame"))){ 
    stop(paste0("The argument ", sQuote("sdf"), " must be an edsurvey.data.frame or a light.edsurvey.data.frame. See the 'Using the ", 
      dQuote("EdSurvey"), " Package's getData Function to Manipulate the NAEP Primer Data vignette' for how to work with data in a light.edsurvey.data.frame.")) 
  } # End of if statment: if sdf does not inherit from edsurvey.data.frame or ... 
  if (inherits(sdf, c("edsurvey.data.frame"))) { # sdf is an edsurvey.data.frame, so return whether string is in sdf$weights
    return(var %in% names(sdf$weights))
  } else { # sdf is a light.edsurvey.data.frame, so return whether string is in attributes(sdf)$weights
    return(var %in% names(attributes(sdf)$weights))
  }
}