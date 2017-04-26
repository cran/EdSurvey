#' @title Creates an edsurvey.data.frame.
#' 
#' @description Opens a connection to the data file residing on the disk and returns information about the file and data.
#' 
#' @param filepath character indicating the location and name of the file.
#' @param defaultWeight character that indicates the default weight.
#' @param defaultPvs character that indicates the default plausible value.
#' @param omittedLevels character vector indicating which factor levels (really, labels)
#'                       should be excluded. When set to the default value of c('Multiple',NA,'Omitted'), adds the vector to the \code{edsurvey.data.frame}.
#' @param frPath character indicating the location of the fr2 parameter file included with the data companion.
#' @details The function uses precompiled label information for data to read in a fixed-width format file, and uses prestored data
#'          to get information about the file. 
#' @return An \code{edsurvey.data.frame} containing the following elements:
#'    \item{userConditions}{A list containing all user conditions set using the \code{subset.edsurvey.data.frame} method.}
#'    \item{defaultConditions}{The default conditions to be applied to the \code{edsurvey.data.frame}.}
#'    \item{data}{An \code{LaF} object containing a connection the student data set on disk.}
#'    \item{dataSch}{An \code{LaF} object containing a connection the school data set on disk.}
#'    \item{weights}{A list containing the weights found on the \code{edsurvey.data.frame}.}
#'    \item{pvvar}{A list containing the plausible values found on the \code{edsurvey.data.frame}.}
#'    \item{subject}{The subject of the data set contained in the \code{edsurvey.data.frame}.}
#'    \item{year}{The year of assessment of the data set contained in the \code{edsurvey.data.frame}.}
#'    \item{assessmentCode}{The code of the data set contained in the \code{edsurvey.data.frame}.}
#'    \item{dataType}{The type of data (whether student or school) contained in the \code{edsurvey.data.frame}.}
#'    \item{gradeLevel}{The grade of the data set contained in the \code{edsurvey.data.frame}.}
#'    \item{achievementLevels}{Default NAEP achievement cutoff scores.}
#'    \item{omittedLevels}{The levels of the factor variables that will be omitted from the \code{edsurvey.data.frame}.}
#'    \item{fileFormat}{A \code{data.frame} containing the parsed information from the student .fr2 file associated with the data.}
#'    \item{fileFormatSchool}{A \code{data.frame} containing the parsed information from the school .fr2 file associated with the data.}
#'    \item{survey}{The type of survey data contained in the \code{edsurvey.data.frame}.}
#' @author Ahmad Emad
#' @example \man\examples\readNAEP.R
#' @export
readNAEP <- function(filepath, defaultWeight = "origwt", defaultPvs = "composite", omittedLevels = c('Multiple',NA,'Omitted'), frPath = NULL) {
  #RFE: we should make sure filepath is a length 1 character, ends with .dat
  filepath = gsub("\\","/", filepath, fixed=TRUE)
  
  splits <- strsplit(filepath, "/")[[1]]
  filename <- splits[length(splits)]
  filename <- tolower(filename)
  filename <- gsub('.dat','', filename) 

  # we want to get rid of the last instance of a folder named data, so reverse
  revFilePath <- paste(rev(strsplit(filepath, NULL)[[1]]), collapse="")
  # then fine the first instance of "data" reversed.
  schPath <- NULL 
  if(tolower(substr(revFilePath,8,8)) == "t") {
    revSchPath <- revFilePath
    substr(revSchPath,8,8) <- "C"
    schPath <-  paste(rev(strsplit(revSchPath, NULL)[[1]]), collapse="")
    if(!file.exists(schPath)) {
      schPath <- NULL
    }
  }
  revfrName <- sub("atad", "smraP/tceleS", tolower(revFilePath), fixed=TRUE) 
  frName <- paste(rev(strsplit(revfrName, NULL)[[1]]), collapse="")
  frName <- gsub(".dat", ".fr2", frName, fixed=TRUE)
  
  if(!is.null(schPath)) {
    revSchfrName <- revfrName
    substr(revSchfrName,8,8) <- "C"
    frSchName <- paste(rev(strsplit(revSchfrName, NULL)[[1]]), collapse="")
    frSchName <- gsub(".dat", ".fr2", frSchName, fixed=TRUE)

  }

  if (!is.null(frPath)) {
    frName <- frPath
    # RFE: deal with school path here too!
  }

  labelsFile <- readMRC(frName)
  dataSchLaf <- NULL
  schLabelsFile <- NULL
  if(!is.null(schPath)) {
    schLabelsFile <- readMRC(frSchName)
    schLabelsFile <- schLabelsFile[order(schLabelsFile$Start),]
    widths <- schLabelsFile$Width
    dataTypes <- as.character(schLabelsFile$dataType)
    varNames = as.character(tolower(schLabelsFile$variableName))
    dataSchLaf <- laf_open_fwf(filename=schPath, column_types=dataTypes,
                               column_names=varNames, column_widths=widths)
  }
  labelsFile <- labelsFile[order(labelsFile$Start),]
  widths <- labelsFile$Width
  dataTypes <- as.character(labelsFile$dataType)
  varNames = as.character(tolower(labelsFile$variableName))
  
  dataLaf <- laf_open_fwf(filename=filepath, column_types=dataTypes,
                          column_names=varNames, column_widths=widths)
 
  #Defining PVs and JKs

  
  ##############################################################
  ## Accomodation not permitted weights and PVs
  pvs = list()
  pv_subset <- subset(labelsFile, select = c('Type','variableName'), labelsFile$Labels %in% c("PV", "PV2"))
  uniquePvTypes = unique(pv_subset$Type)
  for (i in uniquePvTypes) {
    vars <- tolower(pv_subset$variableName[pv_subset$Type == i])
    temp_list <- list(varnames = vars)
    pvs[[i]] <- temp_list
  }
  
  weight_temp <-  tolower(varNames[labelsFile$Labels == "JK"])
  jksuffix <- gsub("[^0-9]","", weight_temp)
  base <- gsub(jksuffix[1],"", weight_temp[1])

  # setup weights
  if(sum("JK2" %in% labelsFile$Labels) == 0) {
    # one set of weights
    weights <- list(origwt=list(jkbase=base, jksuffixes=jksuffix))
    names(weights) <- (varNames[labelsFile$weights])[1] # first weight
  } else{ 
    # there is two sets of weights
    weight_tempAP <-  tolower(varNames[labelsFile$Labels == "JK2"])
    jksuffixAP <- gsub("[^0-9]","", weight_tempAP)
    baseAP <- gsub(jksuffixAP[1],"", weight_tempAP[1])
    weights <- list(origwt=list(jkbase=base, jksuffixes=jksuffix), aorigwt=list(jkbase=baseAP, jksuffixes=jksuffixAP))
    names(weights) <- (varNames[labelsFile$weights])[1:2] # first two weights
  }
  # set default
  if(missing(defaultWeight)) {
    attributes(weights)$default <- (varNames[labelsFile$weights])[1]
  } else {
    attributes(weights)$default <- defaultWeight
  }
  
  
  if(!defaultPvs %in% names(pvs)) {
    warning(paste0("Updating name of default plausible value since ",sQuote(defaultPvs), " not found. Setting to ", sQuote(names(pvs[1])), "."))
    defaultPvs <- names(pvs)[1]
  }
  attributes(pvs)$default <- defaultPvs
  #Getting description of data
  f <- list()
  if(filename != "sdfexample") {
    f <- descriptionOfFile(filename)
  }
  else {
    f[['filename']] <- filename
  }
  levels <- achievementLevelsHelp(f["Grade_Level"], f["Year"], f["Subject"])
  names(levels) <- c("Basic", "Proficient", "Advanced")
  # add reporting sample default condition if the column exists
  if("rptsamp" %in% names(dataLaf)) {
    defaultConditions <- quote(tolower(rptsamp)=="reporting sample")
  }
  else {
    defaultConditions <- NULL
  }
  # build the result list and return
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = list(defaultConditions),
                      data = dataLaf,
                      dataSch = dataSchLaf,
                      weights=weights,
                      pvvars=pvs,
                      subject = f[["Subject"]],
                      year = f[["Year"]],
                      assessmentCode = f[["Assessment_Code"]],
                      dataType = f[["Data_Type"]],
                      gradeLevel = f[["Grade_Level"]],
                      achievementLevels = levels,
                      omittedLevels = list(omittedLevels),
                      fileFormat = labelsFile,
                      fileFormatSchool = schLabelsFile,
                      survey = "NAEP",
                      country= "USA",
                      psuVar="jkunit",
                      stratumVar="repgrp1",
                      jkSumMultiplier=1)
}

# @author Paul Bailey & Ahmad Emad
readMRC <- function(filename) {
  # read NAEP machine readable file.
  # this has layout information on it
  t <- try(mrcFile <- readLines(filename), silent=TRUE)

  # Split mrcFile by "\n"
  mrcFile <- strsplit(mrcFile , "\n", fixed=T)
  mrcFile <- unlist(mrcFile)

  # read in the variables from the file,t his is based on the information ETS
  # shared with us
  variableName <- trimws(substring(mrcFile, 1, 8)) # name of the variable
  Start <- as.numeric(trimws(substring(mrcFile, 9, 12))) # start column in file
  Width <- as.numeric(trimws(substring(mrcFile, 13, 14))) # number of characters in variable
  End <- Start + Width -1 # end column in file
  Decimal <- as.numeric(trimws(substring(mrcFile, 15, 15))) # digits (from the right) to be considered decimals
  Labels <- trimws(substring(mrcFile, 21, 70)) # variable label
  NumValue <- as.numeric(trimws(substring(mrcFile, 89, 90))) # number of numeric codes (e.g. one could would be 1="Yes")

  # parse the numeric codes
  labelValues <- character(length(mrcFile))
  for (j in 1:length(mrcFile)) {
    # for each line:
    Ncodes <- NumValue[j]-1
    if (Ncodes > 0) {
      # if it has numeric codes
      # read in up to 26 character per code, plus 2 characters for the number
      codeValSeq <- seq(91, (91+Ncodes*28), by = 28)
      codeLabelSeq <- seq(93, (93+Ncodes*28), by = 28)
      values <- as.numeric(trimws(substring(mrcFile[j], codeValSeq, codeValSeq+1)))
      labels <- trimws(substring(mrcFile[j], codeLabelSeq, codeLabelSeq+19))
      labelValues[j] <- paste(values, labels, collapse ="^", sep="=")
    }
  }

  # keep the original labels
  oLabels <- Labels
  # Finding the plausible weights and jacknife replicates.
  # normally there is just one set of PVs (with multiple subjects or subscales).
  # When there are multiple sets then one starts with an "A" and is the
  # accommodations permitted values
  Labels[grepl("plausible", tolower(Labels)) & grepl("value", tolower(Labels)) & "A" != substring(variableName, 1, 1)] <- "PV"
  Labels[grepl("plausible", tolower(Labels)) & grepl("value", tolower(Labels)) & "A" == substring(variableName, 1, 1)] <- "PV2"

  # normally there is just one set of weights. When there are multiple sets
  # then one starts with an "A" and is the accommodations permitted values
  Labels[grepl("weight", tolower(Labels)) & grepl("replicate", tolower(Labels)) & "A" != substring(variableName, 1, 1)] <- "JK"
  Labels[grepl("weight", tolower(Labels)) & grepl("replicate", tolower(Labels)) & "A" == substring(variableName, 1, 1)] <- "JK2"
  
  
  pvWt <- character(length(mrcFile)) # the number of the PV (e.g. for a subject or subscale with five PVs this would show values from 1 to 5 for five variables)
  Type <- character(length(mrcFile)) # this is the subject or subscale

  # Check if there is at least one Plausible Value, and then finding their names
  tempValue <- applyPV("PV", Labels, pvWt, oLabels, Type)
  Labels <- tempValue[["Labels"]]
  pvWt <- tempValue[["pvWt"]]
  Type <- tempValue[["Type"]]
  
  tempValue <- applyPV("PV2", Labels, pvWt, oLabels, Type)
  Labels <- tempValue[["Labels"]]
  pvWt <- tempValue[["pvWt"]]
  Type <- tempValue[["Type"]]
  # Check if there is at least one JK replicate.
  
  if(sum(Labels=="JK")>0) {
    # get the number of the JK replicate
    pvWt[Labels=="JK"] <- as.numeric(gsub("[^\\d]+", "", oLabels[Labels=="JK"], perl=TRUE))
    pvWt[Labels=="JK2"] <- as.numeric(gsub("[^\\d]+", "", oLabels[Labels=="JK2"], perl=TRUE))
  }
  
  # identify weights
  labels <- tolower(Labels)
  weights <- ifelse(grepl("wgt",variableName) + grepl("student", labels) + grepl("weight", labels) + grepl("unadjusted", labels) + grepl("overall", labels) + grepl("unpoststratified", labels) - 5 * grepl("replicate", labels) >= 4, TRUE, FALSE)
  # For now, assume all variables are characters.
  dataType <- rep("character", length(mrcFile))
  # Create appropriate data type for variables.
  dataType[Decimal >0 & Width < 8] <- "integer"
  dataType[Decimal >0 & Width >= 8] <- "numeric"
  labelValues <- as.character(labelValues)
  mrcFileCSV <- data.frame(variableName, Start, End, Width, Decimal, Labels, labelValues, pvWt, Type, dataType, weights, stringsAsFactors=FALSE)
  mrcFileCSV <- mrcFileCSV[order(mrcFileCSV$Start),]
  #mrcFileCSV$labelValues <- as.character(mrcFileCSV$labelValues)
  return(mrcFileCSV)
}

# @author Paul Bailey & Ahmad Emad
applyPV <- function(pv, Labels, pvWt, oLabels, Type) {
  # the instances where Labels == pv is the cases we are looking at. 
  if(sum(Labels==pv)>0) {
    # there are some relevant variables

    # example text for oLabels: "Plausible NAEP math value #3 (num & oper)"
    pvWt[Labels==pv] <- sapply(strsplit(oLabels[Labels==pv],"#", fixed=TRUE), function(x) gsub("[^0-9]", "", x[[2]]))
    # make oLabels lower case
    oLabels[Labels == pv] <- tolower(oLabels[Labels==pv])
    
    # edit oLabels to Keep only letters
    # 1) remove punctuation
    oLabels[Labels == pv] <- gsub("[[:punct:]]", "", oLabels[Labels==pv])
    # 2) remove numbers
    oLabels[Labels == pv] <- gsub("[0-9]", "", oLabels[Labels==pv])
    
    # break oLabels into words and remove common words
    temp <- strsplit(oLabels[Labels==pv], " ", fixed=TRUE)
    temp <- lapply(temp, function(x) x[x!=""])
    # this code gets rid of words that are common to all Plausible values,
    # for example, "plausible", "value", and "NAEP"
    words <- temp[[1]]
    summation <- sapply(temp, function(x) words %in% x)
    summation[summation == TRUE] <- 1
    summation[summation == FALSE] <- 0
    summation <- rowSums(summation)
    removeWords <- words[summation == length(temp)]
    # check if there is more than one plausible value (and so all words are "common")
    if(length(removeWords) != length(words)) {
      # Remove common words
      temp <- lapply(temp, function(x) x[!x %in% removeWords])
      # concatenate remaining words with "_"
      temp <- sapply(temp, function(x) paste0(x, collapse="_"))
      Type[Labels == pv] <- temp  
    }
    else {
      # if there is only one, get rid of words like plausible, value, naep
      Type[Labels == pv] <- gsub("plausible|naep|value", "", oLabels[Labels == pv])
      Type[Labels == pv] <- trimws(Type[Labels == pv])
      Type[Labels == pv] <- gsub(" ", "_", Type[Labels == pv])
    }
  }
  Type[Labels == "PV2"] <- paste0(Type[Labels == "PV2"], "_ap")
  return(list(Labels=Labels, Type=Type, pvWt=pvWt))
}
