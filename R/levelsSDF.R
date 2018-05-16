#' @title Print Levels and Labels
#'
#' @description Retrieve the levels and labels of a variable from an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}.
#'
#' @param varnames a vector of character strings to search for in the database connection object (\code{data})
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'         an \code{edsurvey.data.frame.list}
#' 
#' @author Michael Lee and Paul Bailey
#' @example  man/examples/levelsSDF.R
#' @export
levelsSDF <- function(varnames, data) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  # check recode attribute
  userConditions <- getAttributes(data, "userConditions")
  recode <- userConditions[which(names(userConditions) %in% "recode")]
  # if a variable level has been recoded, need to remove the label used to identify it when printing the updated result
  if (length(recode) > 0) {
    recode <- unlist(recode, recursive = FALSE)
    names(recode) <- tolower(gsub("^recode.","",names(recode)))
  }

  if (inherits(data, c("edsurvey.data.frame"))) {
    # data is an edsurvey.data.frame, so levels are found using a subset of data$fileFormat
    varnames <- toupper(varnames)
    labelsFile <- rbind(data$fileFormat, data$fileFormatSchool, data$fileFormatTeacher)
    vars = subset(labelsFile, labelsFile$variableName %in% varnames)
    levelsData <- list()
    for (i in unique(varnames)) {
      varDF <- vars[vars$variableName == i, ]
      varLevels = unlist(strsplit(varDF["labelValues"][[1]], "^", fixed = TRUE))
      # if variable is in the list of recode, need to change the level result
      if (length(recode) > 0) {
        recodeVar <- recode[which(names(recode) == tolower(i))]
        if (length(recodeVar) > 0) {
          for (ni in 1:length(recodeVar)) {
            from <- recodeVar[[ni]]$from
            to <- recodeVar[[ni]]$to
            if (length(to) > 1) {
              stop(paste0("More than one 'To' value found in the ", sQuote(tolower(i)) ," element of the 'recode' argument."))
            }
            varLevels <- gsub(paste(paste0("=",from,"$"), collapse = "|"),paste0("=",to),varLevels)
          }
        } 
      }
      
      levelsData[[i]] <- c(varLevels)
    }
  } else { # If data does not inherit from edusrvey.dataframe
    # data is a light.edsurvey.data.frame, so levels are found in the attrubutes of the llevels
    # attributes of the data.frame
    levelsData <- list()
    varnames <- tolower(varnames)
    for (i in unique(varnames)) {
      lev <- levels(data[[i]])
      lab <- attr(data[[i]], "llevels")
      varLevels <- paste(lab, lev, sep = "=")  # for each unique variable paste the levels and labels separated by '=', matching data$fileFormat
      if (!is.null(recode)) {
        recodeVar <- recode[which(names(recode) == tolower(i))]
        if (length(recodeVar) > 0) {
          for (ni in 1:length(recodeVar)) {
            from <- recodeVar[[ni]]$from
            to <- recodeVar[[ni]]$to
            if (length(to) > 1) {
              stop(paste0("More than one 'To' value found in the ", sQuote(tolower(i)) ," element of the 'recode' argument."))
            }
            varLevels <- gsub(paste(paste0("=",from,"$"), collapse = "|"),paste0("=",to),varLevels)
          }
        } 
      }
      
      levelsData[[i]] <- c(varLevels)
    } # End of if statment: i in unique(varnames)
  } # End of if/else statement: if inherits(data, c("edsurvey.data.frame"))
  names(levelsData) <- tolower(names(levelsData))
  class(levelsData) <- c("levelsSDF")
  if (length(levelsData) == 0) {
    # return a warning if there are no variables in the data$fileFormat or llevels attribute of
    # the data.frame that match the string searched
    warning(paste0("There are no variables with the string ", sQuote(varnames), " in this ", class(data)[[1]], "."))
  }
  levelsData
}

# @author Michael Lee
#' @method print levelsSDF
#' @export
print.levelsSDF <- function(x, ...) {
  x <- lapply(x, gsub, pattern = "=", replacement = ". ")  # replace '=' with '. ' to indicate level
  if (length(x) > 0) {
    for (i in 1:length(x)) {
      cat(paste0("Levels for Variable '", tolower(names(x[i])), "' (Lowest level first):\n"))
      if (identical(x[[i]], character(0))) {
        # if there is no level info return an NA
        cat(paste0("    ", paste0(NA), "\n"))
      } else {
        for (ii in 1:length(x[i][[1]])) {
          # for each unique level, paste the level number and name
          cat(paste0("    ", x[[i]][[ii]], "\n"))
        } # End of for loop ii in 1:length(x[i][[1]])
      } # End of if/else: identical(x[[i]], character(0))
    } # End of loop: i in 1:length(x)
  } # End of If statment: if legth of x is greater than 0
}
