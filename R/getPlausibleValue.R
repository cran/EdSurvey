#' @title Get plausible values from a given subject or subscale.
#'
#' @description Get the set of variables on an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} associated with the given subject or subscale.
#'
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#' @param var a character vector naming the subject scale or subscale.
#'
#' @return A character vector of the set of variable names for the plausible values.
#'
#' @details This function will return a set of variable names for plausible values that
#' \code{\link{hasPlausibleValue}} returns as true.
#'
#' @seealso \code{\link{showPlausibleValues}}
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\getPlausibleValue.R
#' @export
getPlausibleValue <- function(var, data) {
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  if(!hasPlausibleValue(var, data)) {
    stop("The ", sQuote("var"), " argument level of ", dQuote(var), " does not have plausible values.")
  }
  if (inherits(data, c("edsurvey.data.frame"))) {
    # data is an edsurvey.data.frame, so plausible value is returned in data$pvvars[var]
    pvi <- data$pvvars[var]
  } else {
    # data is a light.edsurvey.data.frame, so plausible value is returned in
    # attributes(data)$pvvars[var]
    pvi <- attributes(data)$pvvars[var]
  }
  unname(unlist(pvi))
}
