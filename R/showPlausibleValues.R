#' @title Print a summary of the subject scale or subscale and the associated variables for their plausible values.
#'
#' @description Print out a summary of the subject scale or subscale and the associated variables for their
#'              plausible values on an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#'
#' @param sdf an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#' @param verbose logical. Set to \code{TRUE} to get the variable names for plausible values.
#'
#' @return A pasted statement of the set of variable names for the plausible values.
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\showPlausibleValues.R
#' @export
showPlausibleValues <- function(sdf, verbose = FALSE) {
  if (!inherits(sdf, c("edsurvey.data.frame", "light.edsurvey.data.frame"))){ 
    stop(paste0("The argument ", sQuote("sdf"),
                " must be an edsurvey.data.frame or a light.edsurvey.data.frame. See the 'Using the ", 
                dQuote("EdSurvey"),
                " Package's getData Function to Manipulate the NAEP Primer Data vignette' for how to work with data in a light.edsurvey.data.frame."))
  }

  # 1:i-th plausible value is returned in sdf$pvvars
  pvvars <- getAttributes(sdf, "pvvars")
  pvNames <- names(pvvars)
  cat(paste0("There are ", length(pvNames), " subject scale(s) or subscale(s) in this edsurvey.data.frame\n"))
  for (i in 1:length(pvNames)) {
    if (length(pvNames) == 0) {
      break
    }
    pvi <- pvvars[[i]]
    cat(paste0("  ", sQuote(names(pvvars)[i]),
               " subject scale or subscale with ",
               length(pvi$varnames), 
               " plausible values"))
    if (attributes(pvvars)$default == pvNames[i]) {
        # if there is a default plausible value, return with paste ' (the default)'
      cat(" (the default).")
    } else {
      cat(".")
    } # End of if statment if attributes(sdf$pvvars)$default == pvNames[i]
    if (verbose) {
      # if verbose = TRUE, return all plausible value details for each subject scale/subscale
      cat(" They are:\n    ")
      pvi <- getPlausibleValue(pvNames[i], sdf)
      cat(paste0("'", pvi, "'"))
    } # end of is statment: if verbrose 
    cat("\n\n")
  } # End of loop for i in 1:length(pvNames)
}
