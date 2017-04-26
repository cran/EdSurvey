#' @title Print a summary of achievement-level cut points.
#'
#' @description Print a summary of the achievement-level cut points on an
#'              \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#'
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}.
#'
#' @return A pasted statement that shows the achievement-level cut points
#'         associated with an \code{edsurvey.data.frame}
#'         or \code{light.edsurvey.data.frame}.
#' 
#' @example \man\examples\showCutPoints.R
#' @export
showCutPoints <- function(data) {
  sdf <- data
  if (!inherits(sdf, c("edsurvey.data.frame", "light.edsurvey.data.frame"))) 
    stop(paste0("The argument ", sQuote("data"), " must be an edsurvey.data.frame or a light.edsurvey.data.frame. See the 'Using the ", 
      dQuote("EdSurvey"), " Package's getData Function to Manipulate the NAEP Primer Data vignette' for how to work with data in a light.edsurvey.data.frame."))
  
  cat(paste0("Achievement Levels:\n"))
  als <- getAttributes(sdf, "achievementLevels")
  if (length(als) > 0) {
    cat(paste0("  Basic:  ", als[1], "\n"))
    cat(paste0("  Proficient:  ", als[2], "\n"))
    cat(paste0("  Advanced:  ", als[3], "\n"))
  } else {
    # paste a statement if there are no achievement levels in the sdf
    cat(paste0("  No achievement levels.\n"))
  }
}
