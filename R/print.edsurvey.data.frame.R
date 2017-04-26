#' @title Print out a summary of the edsurvey.data.frame.
#'
#' @description Prints details of an \code{edsurvey.data.frame}.
#' 
#' @param x             an \code{edsurvey.data.frame}.
#' @param printColnames logical. Set to \code{TRUE} to see all column names.
#' @param ... these arguments are not passed anywhere and are included only for compatibility.
#' @return              A pasted statement vector that shows the details of an \code{edsurvey.data.frame} 
#' 
#' @author Michael Lee and Paul Bailey
#' @method print edsurvey.data.frame
#' @export
print.edsurvey.data.frame <- function(x, printColnames = FALSE, ...) {
  if (!class(x) %in% "edsurvey.data.frame") {
    stop(paste0(sQuote("x"), "must be an edsurvey.data.frame"))
    }
  stopifnot(class(x) == "edsurvey.data.frame")
  cat(paste0("edsurvey.data.frame with ", nrow(x), " rows and ", ncol(x), " columns.\n"))

  if (printColnames) {
    cat("Column names:\n")
    print(names(x$data))
  }
  cat("\n")
  
  if (length(x$weights) > 0) {
    showWeights(x, verbose = FALSE)
  }
  
  
  if (length(x$pvvars) > 0) {
    showPlausibleValues(x, verbose = FALSE)
  }
  
  # Describe File attributes
  if (length(x$fileDescription) > 0) {
    cat("Description of data:\n")
    for (i in 1:length(x$fileDescription)) {
      # pastes the i-th name and label for each value in sdf$fileDescription
      description <- x$fileDescription[[i]]
      cat(paste0("  ", names(x$fileDescription)[i], ": ", description, "\n"))
    } # end of for loop: i in 1:length(x$fileDescription)
  } # end of If statment: if length of fileDescrption is greater than 0
  
  # Describe omitted.levels pastes each omitted level, collapsing by ', ' (ex: 'Multiple',
  # 'NA', 'Omitted')
  if (length(x$omittedLevels) > 0) {
    cat(paste0("Omitted Levels: '", paste(x$omittedLevels[[1]], collapse = "', '"), "'\n"))
  }
  cat("\n")
  
  # Describe conditions
  if (length(x$userConditions) > 0) {
    cat("User Conditionss:\n")
    description <- x$userConditions
    cat(paste0("  ", description, collapse = "\n"), "\n")
  }
  cat("\n")
  
  if (length(x$defaultConditions) > 0) {
    cat("Default Conditions:\n")
    description <- x$defaultConditions
    cat(paste0("  ", description, "\n"))
  }
  
  al <- getAttributes(x, "achievementLevels")
  if(length(al) > 0) {
    noms <- names(al)
    cat("Achievement Levels:\n")
    for(i in 1:length(al)) {  
      post <- paste(rep(" ",1+max(nchar(noms)) - nchar(noms[i])), collapse="")
      cat(paste0("  ",noms[i],":",post, al[i], "\n"))
    }
  }
  cat(paste0("\n", "Survey: ", x$survey, "\n"))
}
