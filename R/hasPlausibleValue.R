#' @title Return a value indicating whether this variable has associated plausible values.
#'
#' @description Return a value indicating if this variable has associated plausible values in an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#'
#' @param data an \code{edsurvey.data.frame} or a \code{light.edsurvey.data.frame}.
#' @param var character indicating the variable in question.
#'
#' @return A boolean (or vector when \code{var} is a vector) indicating if each element of \code{var} has
#'         plausible values associated with it. 
#'
#' @details Note that this function returns \code{TRUE} only when the variable passed to it is the name for a set of plausible values, but
#'          not if it is an individual plausible value from such a set. Thus, on the NAEP primer, \code{composite} has plausible
#'          values (and so \code{TRUE} would be returned by this function), but any of the plausible values or variable names defined on
#'          the actual data (such as \code{"mrpcm1"} or \code{"dsex"}) are not.
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\hasPlausibleValue.R
#' @export
hasPlausibleValue <- function(var, data) {
  sdf <- data
  if (!inherits(sdf, c("edsurvey.data.frame", "light.edsurvey.data.frame"))) 
    stop(paste0("The argument ", sQuote("sdf"), " must be an edsurvey.data.frame or a light.edsurvey.data.frame. See the 'Using the ", 
      dQuote("EdSurvey"), " Package's getData Function to Manipulate the NAEP Primer Data vignette' for how to work with data in a light.edsurvey.data.frame."))
  if (inherits(sdf, c("edsurvey.data.frame"))) {
    # sdf is an edsurvey.data.frame, so return whether string is in sdf$pvvars
    return(var %in% names(sdf$pvvars))
  } else {
    # sdf is a light.edsurvey.data.frame, so return whether string is in attributes(sdf)$pvvars
    return(var %in% names(attributes(sdf)$pvvars))
  } # end of if statment: if sdf inherits edsurvey.data.frame
}