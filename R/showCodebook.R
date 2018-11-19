#' @title Summary Codebook
#'
#' @description Retrieves variable names, variable labels, and value labels for an \code{edsurvey.data.frame}, \code{light.edsurvey.data.frame},
#' or \code{edsurvey.data.frame.list}.
#'
#' @param data            an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or
#'                        an \code{edsurvey.data.frame.list}
#' @param fileFormat      a character string indicating the data source to search for variables.
#'                        The default \code{NULL} argument searches all available codebooks in the database connection object.
#' @param labelLevels     a logical value; set to \code{TRUE} to return a snapshot of the label levels in
#'                        an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}. When set to \code{FALSE}
#'                        (the default), label levels are removed.
#' @param includeRecodes  a logical value; set to \code{TRUE} to return value labels that have been recoded in
#'                        an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}. When set to \code{FALSE}
#'                        (the default), only the original value labels are included in the returned \code{data.frame}.
#' @return                a \ifelse{latex}{\code{data.frame}}{\code{\link[base]{data.frame}}} that shows the variable names, variable labels, value labels,
#'                        value levels (if applicable), and the file format data source from an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'                        or an \code{edsurvey.data.frame.list}
#'
#' @author Michael Lee
#' @example \man\examples\showCodebook.R
#' @export
showCodebook <- function(data, fileFormat = NULL, labelLevels = FALSE, includeRecodes = FALSE) {
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),data))
  }
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  sdf <- data
  data <- NULL

  # bind all fileFormats into a list object
  if (inherits(sdf, c("edsurvey.data.frame"))) {
    dataList <- list(fileFormat = sdf$fileFormat, fileFormatSchool = sdf$fileFormatSchool, fileFormatTeacher = sdf$fileFormatTeacher)
    } else {
    dataList <- list(fileFormat = attributes(sdf)$fileFormat, fileFormatSchool = attributes(sdf)$fileFormatSchool, fileFormatTeacher = attributes(sdf)$fileFormatTeacher)
  }

  # include only the data fileFormats that exist in the data connection
  dataList <- dataList[!sapply(dataList,is.null)]

  # create an empty data.frame (vars) that will contain our variable information
  vars <- data.frame()
  # if fileFormat is NULL, retrieve all available fileFormats from connection via getAttributes and append
  if (is.null(fileFormat)) {

    for(i in 1:length(dataList)) {
      vars <- rbind(vars, data.frame(getAttributes(sdf, names(dataList)[i]), fileFormat = names(dataList)[i]))
    }

  } else {
    # fileFormat is defined (must be a combination of student, school, or teacher)
    tolower(fileFormat)

    if (!all(fileFormat %in% c("school" ,"student", "teacher"))) {
      stop(paste0("The ", sQuote("fileFormat"), " argument must either be ", dQuote("student"), " or ", dQuote("school"), " or ", dQuote("teacher"),"."))
    }

    # replace names of values accepted in "fileFormat" argument with those used in the attributes of the data connection
    fileFormat <- gsub("school", "fileFormatSchool", fileFormat)
    fileFormat <- gsub("teacher", "fileFormatTeacher", fileFormat)
    fileFormat <- gsub("student", "fileFormat", fileFormat)
    fileFormatOrig <- fileFormat
    fileFormat <- fileFormat[fileFormat %in% names(dataList)]

    # return a warning if there is no fileFormat attribute information available for this data
    if(length(fileFormat) == 0) {
      stop(paste0("The ", sQuote("fileFormat"), " selected contains no codebook information available for this data."))
    }

    # return a warning if there is no fileFormat attribute information available for a specified format listed in fileFormat argument
    if (any(!fileFormatOrig %in% names(dataList))) {
      warning(paste0("The ", pasteItems(fileFormatOrig[which(!fileFormatOrig %in% names(dataList))]), " codebook is not present on this ", class(sdf)[1], "; returning only ", pasteItems(fileFormatOrig[which(fileFormatOrig %in% names(dataList))]), "."))
    }
    # retrieve all available fileFormats from connection via getAttributes and append
    for(i in 1:length(fileFormat)) {
      vars <- rbind(vars, data.frame(getAttributes(sdf, fileFormat[i]), fileFormat = fileFormat[i]))
    }
  }
  
  if (is.data.frame(vars) & nrow(vars) == 0) {
    # return a warning if there is no codebook information available for this data
    warning(paste0("No codebook information available for this data."))
    return(NULL)
  } else {

    # lower the variable names
    vars$variableName <- tolower(vars$variableName)

    # return only variables relevant to codebook
    varsData = vars[, c("variableName", "Labels", "labelValues", "fileFormat")]
    if ("light.edsurvey.data.frame" %in% class(sdf) == TRUE) {
      varsData <- varsData[varsData$variableName %in% colnames(sdf), ]
    }

    # function used to include recoded levels to the database connection done by the user via recode.sdf
    parseLevelRecodes <- function(data, variableName, variableLevel) {
      for (i in 1:nrow(data)) {
        if (data[[variableLevel]][i] != "") {
          varLevels <- levelsSDF(data[[variableName]][i], sdf)
          varLevels <- unlist(varLevels, use.names = FALSE)
          varLevelsSplit <- c()
          for (ii in 1:length(varLevels)) {
            x <- varLevels[[ii]]
            varLevelsSplit <- c(varLevelsSplit, x)
          }
          varLevelsSplitPaste <- paste(varLevelsSplit, collapse = "^")
          data$labelValueRecodes[[i]] <- varLevelsSplitPaste
        } else {
          data$labelValueRecodes[[i]] <- paste0("")
        }
      }
      data
    }

    # the codebook output includes recoded levels (using recode.sdf) done by the user, adding a column "labelValueRecodes" to the returned data.frame
    if(includeRecodes) {
      varsData <- parseLevelRecodes(varsData, variableName = "variableName", variableLevel = "labelValues")
    }
    
    # if label levels aren't returned AND output should include recodes, parse out the "^" and the value level and replace it with "; "
    if(all(includeRecodes & !labelLevels)) {
      varsData$labelValueRecodes <- as.character(lapply(strsplit(varsData$labelValueRecodes, "^", fixed=TRUE),function(x) {
                paste(sapply(strsplit(x, "\\="), `[`, 2), collapse = "; ")
              }))
      } 

    # if label levels aren't returned, parse out the "^" and the value level and replace it with "; "
    if(!labelLevels) {
      varsData$labelValues <- as.character(lapply(strsplit(varsData$labelValues, "^", fixed=TRUE),function(x) {
                paste(sapply(strsplit(x, "\\="), `[`, 2), collapse = "; ")
              }))
    }

    # if labelLevels are returned, parse out the "^" and replace it with "; "
    if(labelLevels) {
      if(includeRecodes) {
        varsData$labelValueRecodes <-gsub("\\^", "; ", varsData$labelValueRecodes)
      } 
      varsData$labelValues <-gsub("\\^", "; ", varsData$labelValues)
    } 
  }
  return(varsData)
}
