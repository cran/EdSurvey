#' @title EdSurvey Metadata Summary
#'
#' @description Prints metadata regarding an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#' 
#' @param x             an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#' @param printColnames a logical value; set to \code{TRUE} to see all column names in the \code{edsurvey.data.frame}
#'                      or the \code{edsurvey.data.frame.list}
#' @param ... these arguments are not passed anywhere and are included only for compatibility
#' 
#' @author Michael Lee and Paul Bailey
#' @method print edsurvey.data.frame
#' @aliases print.edsurvey.data.frame.list
#' @export
print.edsurvey.data.frame <- function(x, printColnames = FALSE, ...) {
  if (!inherits(x, "edsurvey.data.frame")) {
    stop(paste0(sQuote("x"), " must be an edsurvey.data.frame"))
  }
  dm <- dim(x)
  cat(paste0("edsurvey.data.frame with ", dm[1], " rows and ", dm[2], " columns.\n"))

  if (printColnames) {
    cat("Column names:\n")
    print(names(x$data))
  }
  cat("\n")
  
  if (length(x$weights) > 0) {
    showWeights(x, verbose = FALSE)
  }
  
  
  if (length(x$pvvars) > 0) {
    cat("\n")
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
    cat("\n")
    cat(paste0("Omitted Levels: '", paste(unlist(x$omittedLevels), collapse = "', '"), "'\n"))
  }
  
  # Describe user in put conditions
  if (length(x$userConditions) > 0) {
    cat("\n")
    cat("User Conditions:\n")
    description <- x$userConditions
    cat(paste0("  ", description, collapse = "\n"), "\n")
  }
  
  if (length(x$defaultConditions) > 0) {
    cat("\n")
    cat("Default Conditions:\n")
    description <- x$defaultConditions
    cat(paste0("  ", description, "\n"))
  }
  
  if (length(x$recodes) > 0) {
    cat("\n")
    cat("Recodes:\n")
    description <- x$recodes
    cat(paste0("  ", description, "\n"))
  }

  al <- getAttributes(x, "achievementLevels")
  ### Handle more than 1 achievement level scales
  if(is.list(al)) { 
    for (ali in 1:length(al)) {
      cat("\nAchievement Levels:\n")
      cat(paste0(names(al)[ali],": \n"))
      if(length(al[[ali]]) == 0) {
        cat("  Achievement levels for this subject is not defined this year. \n") 
      } else {
        noms <- names(al[[ali]])
        for(i in 1:length(al[[ali]])) {  
          post <- paste(rep(" ",1+max(nchar(noms)) - nchar(noms[i])), collapse="")
          cat(paste0("  ",noms[i],":",post, sprintf("%.2f",al[[ali]][i]), "\n"))
        }  
      }
    }
  } else {
    if(length(al) > 0) {
      noms <- names(al)
      cat("\nAchievement Levels:\n")
      for(i in 1:length(al)) {  
        post <- paste(rep(" ",1+max(nchar(noms)) - nchar(noms[i])), collapse="")
        cat(paste0("  ",noms[i],":",post, al[i], "\n"))
      }
    }
  }

  cat(paste0("\nSurvey: ", x$survey, "\n"))
}

#' @export
#' @method print edsurvey.data.frame.list
print.edsurvey.data.frame.list <- function(x, printColnames = FALSE, ...) {
  li <- length(x$data)
  cat(paste0("an edsurvey.data.frame.list with ", li, " elements\n"))
  cat("covs:\n")
  print(x$covs)
  for(i in 1:li) {
    cat(paste0("\n\nElement ",i,"\n\n"))
    print(x$data[[i]])
  }
}
