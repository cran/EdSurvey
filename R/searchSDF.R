#' @title Search and print out a summary of variable information on an edsurvey.data.frame or light.edsurvey.data.frame.
#'
#' @description \code{searchSDF} returns a \ifelse{latex}{\code{data.frame}}{\code{\link[base]{data.frame}}} that shows the variable names and
#' labels from an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}. The function will search both student
#' and school data sets for a matching character string in embedded file format.
#'
#' @param string     character string to search for in the database connection object (\code{data}).
#'                   Note that the function will search both the student
#'                   and school datasets for a matching character string in embedded 
#'                   file format.
#' @param data object of class \code{edsurvey.data.frame} (see \code{\link{readNAEP}}
#'                   for how to generate an \code{edsurvey.data.frame}).
#' @param fileFormat character string indicating the file type to search for variables.
#'                   The default \code{NULL} argument searches both the student and school files.
#'                   If the string value isn't \dQuote{school} or \dQuote{student}, the
#'                   function will return an error message indicating an empty match.
#' @param levels     logical. Set to \code{TRUE} to return a snapshot of the levels in
#'                   an \code{edsurvey.data.frame}.
#' @return           A \ifelse{latex}{\code{data.frame}}{\code{\link[base]{data.frame}}} that shows the variable names, labels,
#'                   and levels (if applicable) from an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame} based on a matching character string.
#'
#' @author Michael Lee
#' @example \man\examples\searchSDF.R
#' @export
searchSDF <- function(string, data, fileFormat = NULL, levels = FALSE) {
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  sdf <- data
  data <- NULL
  if (is.null(fileFormat)) {
    # bind and search fileFormats in both student and school if not defined
    if (inherits(sdf, c("edsurvey.data.frame"))) {
      labelsFile <- rbind(sdf$fileFormat, sdf$fileFormatSchool)
      vars = labelsFile[grepl(string, labelsFile$variableName, ignore.case = TRUE) | 
                        grepl(string, labelsFile$Labels, ignore.case = TRUE),] 
      #vars = subset(labelsFile, grepl(string, labelsFile$variableName, ignore.case = TRUE) | 
      #  grepl(string, labelsFile$Labels, ignore.case = TRUE))
    } else {
      warning(paste0("Searched for string ", sQuote(string),
                     " only in this light.edsurvey.data.frame. To search the full data set, change ", 
                     paste0(sQuote("data")), " argument to the edsurvey.data.frame."))
      labelsFile <- rbind(attributes(sdf)$fileFormat, attributes(sdf)$fileFormatSchool)
      labelsFile <- labelsFile[(labelsFile$variableName %in% toupper(names(sdf))), ]
      vars = labelsFile[grepl(string, labelsFile$variableName, ignore.case = TRUE) | 
                        grepl(string, labelsFile$Labels, ignore.case = TRUE),] 
      #vars = subset(labelsFile, grepl(string, labelsFile$variableName, ignore.case = TRUE) | 
      #  grepl(string, labelsFile$Labels, ignore.case = TRUE))
    }
  } else {
    # fileFormat is defined (must be either student or school) and subset based on a string
    tolower(fileFormat)
    if (!fileFormat %in% c("school" ,"student")) {
      stop(paste0("The ", sQuote("fileFormat"), " argument must either be ", dQuote("student"), " or ", dQuote("school"),"."))
    }
    
    if (inherits(sdf, c("edsurvey.data.frame"))) {
      # sdf is an edsurvey.data.frame, so fileFormat is subset using sdf$fileFormat
      if (fileFormat == "student") {
        vars = sdf$fileFormat[grepl(string, sdf$fileFormat$variableName, ignore.case = TRUE) | 
                              grepl(string, sdf$fileFormat$Labels, ignore.case = TRUE),]
        #vars = subset(sdf$fileFormat, grepl(string, sdf$fileFormat$variableName, ignore.case = TRUE) | 
        #  grepl(string, sdf$fileFormat$Labels, ignore.case = TRUE))
      }
      if (fileFormat == "school") {
        if (is.null(sdf$fileFormatSchool)) {
          stop(paste0("The argument ", sQuote("data"), " doesn't contain a school file."))
        } else {
          vars = sdf$fileFormatSchool[grepl(string, sdf$fileFormatSchool$variableName, ignore.case = TRUE) |
                                      grepl(string, sdf$fileFormatSchool$Labels, ignore.case = TRUE),]
          #vars = subset(sdf$fileFormatSchool, grepl(string, sdf$fileFormatSchool$variableName, 
          #ignore.case = TRUE) | grepl(string, sdf$fileFormatSchool$Labels, ignore.case = TRUE))
        }
      }
    } else {
      if (fileFormat == "student") {
        # sdf is a light.edsurvey.data.frame, so fileFormat is subset using
        # attributes(sdf)$fileFormat
        vars = attributes(sdf)$fileFormat[grepl(string, attributes(sdf)$fileFormat$variableName, ignore.case = TRUE) |
                                          grepl(string, attributes(sdf)$fileFormat$Labels, ignore.case = TRUE),]
        #vars = subset(attributes(sdf)$fileFormat, grepl(string, attributes(sdf)$fileFormat$variableName, 
        #  ignore.case = TRUE) | grepl(string, attributes(sdf)$fileFormat$Labels, ignore.case = TRUE))
      }
      if (fileFormat == "school") {
        if (is.null(attributes(sdf)$fileFormatSchool)) {
          stop(paste0("The argument ", sQuote("data"), " doesn't contain a school file."))
        } else {
          vars = attributes(sdf)$fileFormatSchool[grepl(string, attributes(sdf)$fileFormatSchool$variableName, ignore.case = TRUE) |
                                                  grepl(string, attributes(sdf)$fileFormatSchool$Labels, ignore.case = TRUE),]
          #vars = subset(attributes(sdf)$fileFormatSchool, grepl(string, attributes(sdf)$fileFormatSchool$variableName, 
          #ignore.case = TRUE) | grepl(string, attributes(sdf)$fileFormatSchool$Labels, 
          #ignore.case = TRUE))
        }
      }
    }
  }
  
  if (is.data.frame(vars) & nrow(vars) == 0) {
    # return a warning if there are no variables in the fileFormat that match the string
    # searched
    warning(paste0("There are no variables containing the string ", dQuote(string), " in this edsurvey.data.frame or light.edsurvey.data.frame."))
  } else {
    # return variables that match the string searched
    vars$variableName <- tolower(vars$variableName)
    vars$Levels <- NA
    if (levels == TRUE) {
      # return levels of each of the variables
      if (string == "") {
        stop(paste0("The argument ", sQuote("string"), " must be nonempty to return variable levels."))
      }
      varsData = vars[, c("variableName", "Labels", "labelValues", "Levels")]
      if ("light.edsurvey.data.frame" %in% class(sdf) == TRUE) {
        varsData <- varsData[varsData$variableName %in% names(sdf), ]
      }
      for (i in 1:length(varsData$variableName)) {
        # return levels of each of the variables; involves splitting and appending levels from the
        # fileFormat file some variable don't have levels and are read in as ''
        if (varsData$labelValues[[i]] != "") {
          varLevels <- levelsSDF(varsData$variableName[[i]], sdf)
          varLevels <- unlist(varLevels, use.names = FALSE)
          varLevels <- gsub(pattern = "=", replacement = ". ", varLevels)
          varLevelsSplit <- c()
          for (ii in 1:length(varLevels)) {
            x <- varLevels[[ii]]
            varLevelsSplit <- c(varLevelsSplit, x)
          }
          varLevelsSplitPaste <- paste(varLevelsSplit, collapse = "; ")
          varsData$Levels[[i]] <- varLevelsSplitPaste
        } else {
          varsData$Levels[[i]] <- paste0(NA)
        }
      }
      varsData = varsData[, c("variableName", "Labels", "Levels")]
      class(varsData) <- c("searchSDF", "data.frame")
    } else {
      # variable levels aren't returned
      vars$variableName <- tolower(vars$variableName)
      varsData = vars[, c("variableName", "Labels")]
      varsData = data.frame(varsData, stringsAsFactors = FALSE, row.names = NULL)
    }
    varsData[] <- lapply(varsData, as.character)
    varsData
  }
}

#' @method print searchSDF
#' @export
print.searchSDF <- function(x, ...) {
  class(x) <- "data.frame"
  cols <- colnames(x)
  if ("Levels" %in% cols) {
    x[] <- lapply(x, as.character)
    for (i in 1:length(unique(x$variableName))) {
      # loop print function over each unique variable returned in searchSDF
      cat(paste("Variable: ", tolower(x[i, "variableName"]), "\n", sep = ""))  # paste the variable name
      cat(paste("Label: ", x[i, "Labels"], "\n", sep = ""))  # paste the label name
      if (x$Levels[i] == "NA") {
        cat(paste("\n", sep = ""))
      } else {
        cat(paste("Levels (Lowest level first):\n ", sep = ""))
        labs <- lapply(x$Levels, strsplit, split = ";")
        for (ii in 1:length(labs[[i]][[1]])) {
          # for each unique level, paste the level number and name
          cat(paste("    ", labs[[i]][[1]][ii], "\n", sep = ""))
        }
      }
    }
  } else {
    x[] <- lapply(x, as.character)
    cat(paste(x))
  }
}
