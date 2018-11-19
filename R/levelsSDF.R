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
  # show levels only if varnames is valid
  if(any(!varnames %in% colnames(data))) {
    warning(paste0("Could not find variable(s) ", pasteItems(dQuote(varnames), final="or")))
    varnames <- varnames[varnames %in% colnames(data)]
  }

  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  # check recode attribute in userConditions, and only include those recodes that are applicable to varnames called in levelsSDF
  userConditions <- getAttributes(data, "userConditions")
  recode <- userConditions[which(names(userConditions) %in% "recode")]
  recodeVars <- lapply(recode, unlist(names))
  recodeVars <- unique(unlist(recodeVars,use.names = FALSE))
  recode <- recode[ toupper(recodeVars) %in% toupper(varnames)]

  if (length(recode) > 0) {
    recode <- unlist(recode, recursive = FALSE)
    names(recode) <- tolower(gsub("^recode.","",names(recode)))
  }

  if (inherits(data, c("light.edsurvey.data.frame"))) {
    warning("These codes were taken when getData was called and may have been modified since then.")
    varnames <- toupper(varnames)
    labelsFile <- showCodebook(data, labelLevels = TRUE)
    labelsFile$labelValues <- gsub("\\; ", "^", labelsFile$labelValues)
  } else {
    varnames <- toupper(varnames)
    labelsFile <- rbind(data$fileFormat, data$fileFormatSchool, data$fileFormatTeacher)
  }
  labelsFile$variableName <- toupper(labelsFile$variableName)
  vars <- subset(labelsFile, labelsFile$variableName %in% varnames)

  levelsData <- list()
  for (i in unique(varnames)) {
    varDF <- vars[vars$variableName == i, ]
    varLevels <- unlist(strsplit(varDF["labelValues"][[1]], "^", fixed = TRUE))
    # if variable is in the list of recode, need to change the level result
    levelsData[[i]] <- c(varLevels)
  }

  # only recode labels/levels if there are recodes in the user conditions
  if (length(recode) > 0) {

    # if a variable level has been recoded, need to remove the label used to identify it when printing the updated result
    levelsDataRecode <- list()
    for (i in 1:length(recode)) {

      ni <- toupper(names(recode[i]))
      levs <- c()
      for(j in 1:length(unlist(levelsData[ni]))) {
        levs <- c(levs, strsplit(levelsData[[ni]], "=")[[j]][1])
      }

      labs <- c()
      for(j in 1:length(unlist(levelsData[ni]))) {
        labs <- c(labs, strsplit(levelsData[[ni]], "=")[[j]][2])
      }

      from <- recode[[i]]$from
      to <- recode[[i]]$to

      badFrom <- c() #levels with incorrect recodes 
      if (is.numeric(to)) {
        if (!to %in% levs) {
          labs <- c(labs, as.character(to)) # since there are no labels provided, we will use character format of levels
          levs <- c(levs, to)
        }
        toNum <- to
        to <- labs[levs==to]
      } else {
        if (!to %in% labs) {
          labs <- c(labs,to)
          levs <- as.numeric(levs)
          toNum <- max(levs,na.rm=TRUE) + 1
          levs <- c(levs, toNum)
        } else {
          toNum <- levs[which(to %in% labs)]
        }
      } # end if (is.numeric(to)) 
      # after the code above, to is always a character label

      # from can be a vector of mixed numeric and character values
      # fromNum: numeric values in from
      # fromChar: character values in from
      suppressWarnings(fromNum <- as.numeric(from)) # numeric from variables
      fromChar <- from[is.na(fromNum)]# character from variables
      # numeric from variables
      fromNum <- fromNum[!is.na(fromNum)]
      
      # changing tmp according to numeric values of from
      if(length(fromNum)>0) {
        if(any(!fromNum %in% levs)) {
          #add any missing levels to missing list
          badFrom <- fromNum[!fromNum %in% levs]
        }
        labs <- labs[!levs %in% setdiff(fromNum,toNum)]
        levs <- levs[!levs %in% setdiff(fromNum,toNum)]
      }
      # changing tmp according to character values of from
      if(length(fromChar)>0) {
        if(any(!fromChar %in% labs)) {
          badFrom <- c(badFrom, fromChar[!fromChar %in% labs])
        }
        levs <- levs[!labs %in% setdiff(fromChar, to)]
        labs <- labs[!labs %in% setdiff(fromChar, to)]
      }
      varLevels <- paste0(levs,"=",labs)
      
      # if variable is in the list of recode, need to change the level result
      levelsData[[ni]] <- c(varLevels)
    } # for (i in 1:length(recode))
  } # if (length(recode) > 0)
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
