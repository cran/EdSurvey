#' @title Print out a summary of weights.
#'
#' @description Print out a summary of the weights in an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#'
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#' @param verbose logical. Set to TRUE to get jackknife replicate weights.
#'
#' @author Michael Lee and Paul Bailey
#' @example \man\examples\showWeights.R
#' @export
showWeights <- function(data, verbose = FALSE) {
  if (!inherits(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))) {
    stop(paste0("The argument ",
                sQuote("data"),
                " must be an edsurvey.data.frame or a light.edsurvey.data.frame. See the ",
                sQuote(paste0("Using the ", 
                              dQuote("EdSurvey"),
                              " Package's getData Function to Manipulate the NAEP Primer Data vignette")),
                " for how to work with data in a light.edsurvey.data.frame."))
    } #End of if statment: !inherits(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  #  1:i-th weight is returned in weights
  weights <- getAttributes(data, "weights")
  wgtNames <- names(weights)
  cat(paste0("There are ", length(wgtNames), " full sample weight(s) in this edsurvey.data.frame\n"))
  for (i in 1:length(wgtNames)) {
    wgti <- weights[[i]]
    cat(paste0("  ", sQuote(names(weights)[i]), " with ", length(wgti$jksuffixes), " JK replicate weights"))
    if (attributes(weights)$default == wgtNames[i]) {
      # if there is a default weight, return with paste ' (the default)'
      cat(" (the default).")
    } else {
      cat(".")
    } # End of if/esle statment: if attributes(weights)$default == wgtNames[i]
    
    if (verbose) {
      # if verbose = TRUE, return the jackknife replicate weights using the default weight
      cat(" Jackknife replicate weight variables:\n")
      jki <- getWeightJkReplicates(wgtNames[i], data)
      print(jki)
    } # end of if statment: if verbrose 
    cat("\n")
  } #End of For loop: for i in 1:length(wgtNames)
}
