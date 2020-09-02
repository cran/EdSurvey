#' @title Connect to NAEP Data
#'
#' @description Opens a connection to an NAEP data file residing
#'              on the disk and returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#' 
#' @param path a character value indicating the full filepath location and name
#'             of the (.dat) data file
#' @param defaultWeight a character value that indicates the default weight
#'                      specified in the resulting \code{edsurvey.data.frame}.  
#'                      Default value is \code{origwt} if not specified.
#' @param defaultPvs a character value that indicates the default plausible value
#'                   specified in the resulting \code{edsurvey.data.frame}.
#'                   Default value is \code{composite} if not specified.
#' @param omittedLevels a character vector indicating which factor levels/labels
#'                      should be excluded. When set to the default value of
#'                      \code{c('Multiple',NA,'Omitted')}, adds the vector to
#'                      the \code{edsurvey.data.frame}.
#' @param frPath a character value indicating the location of the \code{fr2}
#'               parameter layout file included with the data companion to
#'               parse the specified \code{filepath} data file
#' @details
#' The function uses the \code{frPath} file layout (.fr2) data to read in the
#' fixed-width data file (.dat) and builds the \code{edsurvey.data.frame}.
#'           
#' @return An \code{edsurvey.data.frame} containing the following elements:
#'    \item{userConditions}{a list containing all user conditions set using the \code{subset.edsurvey.data.frame} method}
#'    \item{defaultConditions}{the default conditions to be applied to the \code{edsurvey.data.frame}}
#'    \item{data}{an \code{LaF} object containing a connection to the student dataset on disk}
#'    \item{dataSch}{an \code{LaF} object containing a connection to the school dataset on disk}
#'    \item{dataTch}{not applicable for NAEP data; returns \code{NULL}}
#'    \item{weights}{a list containing the weights found on the \code{edsurvey.data.frame}}
#'    \item{pvvar}{a list containing the plausible values found on the \code{edsurvey.data.frame}}
#'    \item{subject}{the subject of the dataset contained in the \code{edsurvey.data.frame}}
#'    \item{year}{the year of assessment of the dataset contained in the \code{edsurvey.data.frame}}
#'    \item{assessmentCode}{the code of the dataset contained in the \code{edsurvey.data.frame}}
#'    \item{dataType}{the type of data (whether student or school) contained in the \code{edsurvey.data.frame}}
#'    \item{gradeLevel}{the grade of the dataset contained in the \code{edsurvey.data.frame}}
#'    \item{achievementLevels}{default NAEP achievement cutoff scores}
#'    \item{omittedLevels}{the levels of the factor variables that will be omitted from the \code{edsurvey.data.frame}}
#'    \item{fileFormat}{a \code{data.frame} containing the parsed information from the student .fr2 file associated with the data}
#'    \item{fileFormatSchool}{a \code{data.frame} containing the parsed information from the school .fr2 file associated with the data}
#'    \item{fileFormatTeacher}{not applicable for NAEP data; returns \code{NULL}}
#'    \item{survey}{the type of survey data contained in the \code{edsurvey.data.frame}}
#' @author Tom Fink and Ahmad Emad
#' @example \man\examples\readNAEP.R
#' @export
readNAEP <- function(path, defaultWeight = "origwt", defaultPvs = "composite", omittedLevels = c('Multiple',NA,'Omitted'), frPath = NULL) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  filepath <- normalizePath(unique(path), winslash = "/")
  if(length(filepath) != 1) {
    stop(paste0("The argument ", sQuote("filepath"), " must specify exactly one file."))
  }
  # the directory for the file
  filedir <- dirname(filepath)
  # the file name (less a trailing .dat, if included)
  filename <- gsub('.dat$','', basename(filepath))
  if(tolower(substr(filename, nchar(filename) - 3, nchar(filename) - 3)) %in% "c") {
    filename_school <- filename
    substr(filename, nchar(filename) - 3, nchar(filename) - 3) <- "T"
    filepath <- sub(filename_school, filename, filepath)
    warning("Input file was a school level file. Reading in student level file instead. EdSurvey will automatically link the school file.")
  }
  # find the school path, if this is a student file
  schPath <- NULL 
  if(tolower(substr(filename, nchar(filename) - 3, nchar(filename) - 3)) %in% "t") {
    schFilename <- filename
    substr(schFilename, nchar(filename) - 3, nchar(filename) - 3) <- "C"
    schPath <-  paste0(filedir, "/", schFilename,".dat" )
    # fix case on paths
    success <- tryCatch({schPath <- ignoreCaseFileName(schPath)
                          TRUE
                        }, error = function(e){
                          FALSE
                        }, warning = function(w){
                          FALSE 
                        })
    
    if(!file.exists(schPath) || !success) {
      schPath <- NULL
    }
  }
  
  # grab the subfolder select/parms in an case insensitive way 
  if (is.null(frPath)) {
    # Remove special characters in the filedir
    filedirEscapeRegex <- dirname(filedir)
    for (c in strsplit(".*+?^${}()|[]", "")[[1]]) {
      filedirEscapeRegex <- gsub(pattern = c, replacement = paste0("\\", c), filedirEscapeRegex, fixed = TRUE)
    }
    frName <- grep(paste0(filedirEscapeRegex,"/select/parms$"), list.dirs(dirname(filedir)), value=TRUE, ignore.case=TRUE)
    if(length(frName) == 0) {
      stop(paste0("Could not find folder ", dQuote(paste0(dirname(filedir),"/select/parms/")), "." ))
    }
    if(length(frName) > 1) {
      stop(paste0("Folder ",
                  dQuote(paste0(dirname(filedir),"/select/parms/")),
                  " is not unique when searched in a case insensitive way. Please limit the subfolders of ",
                  dQuote(dirname(filedir)), " so that only one is ",
                  dQuote("/select/parms/"), "." ))
    }
    frName <- paste0(frName, "/", filename, ".fr2")
    # fix case on path, only if not user specified
    frName <- ignoreCaseFileName(frName)
  } else {
    frName <- frPath
  }
  
  if(!is.null(schPath)) {
    frSchName <- frName
    substr(frSchName, nchar(frSchName) -7, nchar(frSchName) - 7) <- "C"
    # fix case on path
    frSchName <- ignoreCaseFileName(frSchName)
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
  
  if(is.null(defaultPvs) || all(is.na(defaultPvs))){
    warning(paste0("Argument ", sQuote("defaultPvs"), " not specified. There will not be a default PV value."))
  }else{
    if(!defaultPvs[1] %in% names(pvs)){
      if(length(pvs)>0){
        warning(paste0("Updating name of default plausible value since ",sQuote(defaultPvs[1]), " not found. Setting to ", sQuote(names(pvs[1])), "."))
        defaultPvs <- names(pvs)[1]
      }else{
        warning(paste0("No plausible values found. If plausible value(s) expected, check the ", sQuote("defaultPvs"), " argument."))
      }
    }#end if(!defaultPvs %in% names(pvs))
  }

  if(length(pvs)>0){
    attributes(pvs)$default <- defaultPvs[1]
  }
  
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
                      dataList = buildNAEP_dataList(dataLaf,
                                                    labelsFile,
                                                    dataSchLaf,
                                                    schLabelsFile),
                      weights = weights,
                      pvvars = pvs,
                      subject = f[["Subject"]],
                      year = f[["Year"]],
                      assessmentCode = f[["Assessment_Code"]],
                      dataType = f[["Data_Type"]],
                      gradeLevel = f[["Grade_Level"]],
                      achievementLevels = levels,
                      omittedLevels = omittedLevels,
                      survey = "NAEP",
                      country = "USA",
                      psuVar = "jkunit",
                      stratumVar = "repgrp1",
                      jkSumMultiplier = 1,
                      fr2Path=frName)
}

# @author Paul Bailey & Ahmad Emad
readMRC <- function(filename) {
  # read NAEP machine readable file.
  # this has layout information on it
  t <- try(mrcFile <- readLines(filename), silent=TRUE)

  # Split mrcFile by "\n"
  mrcFile <- strsplit(mrcFile , "\n", fixed=TRUE)
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
  zeroLenVars <- nchar(variableName) == 0
  if(any(zeroLenVars)) {
    warning(paste0("Unnamed variables in .fr2 file on row(s) ", pasteItems( (1:length(variableName))[zeroLenVars]), ". File located at ", filename, ". These variables renamed sequentially, starting with V1."))
    variableName[zeroLenVars] <- paste0("v", 1:sum(zeroLenVars))
  }

  zeroLenVars <- nchar(variableName) == 0
  if(any(zeroLenVars)) {
    warning(paste0("Unnamed variables in .fr2 file on row(s) ", pasteItems( (1:length(variableName))[zeroLenVars]), ". File located at ", filename, ". These variables renamed sequentially, starting with V1."))
    variableName[zeroLenVars] <- paste0("v", 1:sum(zeroLenVars))
  }
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
  weights <- ifelse(4*grepl("origwt", variableName,ignore.case=TRUE) + grepl("wgt",variableName) + grepl("student", labels) + grepl("weight", labels) + grepl("unadjusted", labels) + grepl("overall", labels) + grepl("unpoststratified", labels) - 5 * grepl("replicate", labels) >= 4, TRUE, FALSE)
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


# for a case-sensitive file system this will change the path to resolve
# correctly when the name is known up to the case. 
# Assumes that there is not another file with the same name in the directory 
# when ignoring the case. Essentially, this makes a case-sensitive file
# system work as if case-insensitive (when file names would not be overloaded)
# Note that for the NAEP data this is a safe assumption.
# @author Paul Bailey
ignoreCaseFileName <- function(f) {
  bn <- basename(f) # the name of the file in the folder
  dr <- dirname(f) # the name of the directory
  ff <- list.files(dr, pattern=bn, ignore.case=TRUE, full.names=TRUE)
  if(length(ff) == 0) {
    stop(paste0("Could not find file ", dQuote(f),"."))
  }
  if(length(ff) > 1) {
    stop(paste0("File name ", dQuote(f)," is ambigious with respect to capitalization. Please remove one of the following files and try again.", pasteItems(dQuote(ff), final="or"),"."))
  }
  ff
}

#builds the NAEP dataList object
buildNAEP_dataList <- function(stuLaf, stuFF, schLaf, schFF){
  
  dataList <- list()
  
  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Student"]] <- dataListItem(lafObject = stuLaf,
                                        fileFormat = stuFF,
                                        levelLabel = "Student",
                                        forceMerge = TRUE,
                                        parentMergeLevels=NULL,
                                        parentMergeVars = NULL,
                                        mergeVars = NULL,
                                        ignoreVars = NULL,
                                        isDimLevel = TRUE)
  
  #school datafile won't always be present, only add it if applicable
  if(!is.null(schLaf)){
      dataList[["School"]] <- dataListItem(lafObject = schLaf,
                                           fileFormat = schFF,
                                           levelLabel = "School",
                                           forceMerge = FALSE,
                                           parentMergeLevels = c("Student"),
                                           parentMergeVars = c("scrpsu"),
                                           mergeVars = c("sscrpsu"),
                                           ignoreVars = NULL,
                                           isDimLevel = FALSE)
  }
  
  return(dataList)
}

