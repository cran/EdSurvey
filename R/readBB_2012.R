#' @title Connect to B&B 2008-2012 Data
#'
#' @description Opens a connection to a Baccalaureate & Beyond 2008-2012 data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the root directory path containing the \code{csvFilename}, \code{formatFilename}, and \code{metadataFilename} data files.
#' @param csvFilename a character value of the derived datafile (*.csv) containing the raw B&B 2008-2012 data.
#' @param formatFilename a character value of the format file (*.txt) that describes the layout of the \code{csvFilename}.
#' @param metadataFilename a character value of the metadata file (*.txt) that describes additional metadata of the \code{csvFilename}.
#'                    
#' @details Reads in the specified \code{csvFilename} file for the B&B 2008-2012 longitudinal survey to an \code{edsurvey.data.frame}.
#' 
#' @return
#'  an \code{edsurvey.data.frame} for the B&B 2008-2012 longitudinal dataset.
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, and \code{\link{getData}}
#' @author Tom Fink
#' @example /man/examples/readBB_2012.R
#' @export
readBB_2012 <- function(path,
                        csvFilename = "b12derived_datafile.csv",
                        formatFilename = "b12derived_format.txt",
                        metadataFilename = "b12derived_metadata.txt") {
  
  if(!dir.exists(path)){
    stop(paste0("Cannot find specified folder path ", sQuote(path), "."))
  }
  if(!file.exists(file.path(path, csvFilename))){
    stop(paste0("Cannot find specified data file ", sQuote("csvFilename"), " in path ", sQuote(file.path(path, csvFilename)), "."))
  }
  if(!file.exists(file.path(path, formatFilename))){
    stop(paste0("Cannot find specified data file ", sQuote("formatFilename"), " in path ", sQuote(file.path(path, formatFilename)), "."))
  }
  if(!file.exists(file.path(path, metadataFilename))){
    stop(paste0("Cannot find specified data file ", sQuote("metadataFilename"), " in path ", sQuote(file.path(path, metadataFilename)), "."))
  }
  
  fileFormat <- getMetaFormatDictionary(file.path(path, metadataFilename), file.path(path, formatFilename)) #get the file format based on the fileformat file and metadata file
  
  lafObj <- laf_open_csv(file.path(path, csvFilename), fileFormat$dataType, fileFormat$variableName, skip=1) #ensure to skip header row
  
  fileFormat <- identifyBBWeights_2012(fileFormat)
  weights <- buildBBWeightList_2012(fileFormat)

  pvs <- list() #no plausible values or achievement levels?
  omittedLevels <- c("{Missing}", "{Not applicable}",
                     "{Skipped}", "{Did not enroll after bachelor's}",
                     "{Not administered - abbreviated}", "{No post-bachelor's enrollment}",
                     "{Instrument error}", "{Out of range}",
                     "{Not classified}", "{Independent student}", 
                     "{Multiple values possible}", "(Missing)", NA)

  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildBBDataList_2012(lafObj, fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "",
                      year = "2008-2012",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "B&B",
                      country = "USA",
                      psuVar = NULL,  #psu and stratum are weight specific
                      stratumVar = NULL, 
                      jkSumMultiplier = 1,
                      validateFactorLabels = FALSE, #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
                      reqDecimalConversion = FALSE) #decimal conversion is not needed
}


#identified the ELS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyBBWeights_2012 <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #
  wgtVars <- grep("^wt(a|b|c|d|e|f)000$", varNames, value=TRUE, ignore.case = TRUE)
  
  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildBBWeightList_2012 <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i] #full variable name of the weight var
    baseWgtVar <- substr(tempVar, 1, 3) #strip off the 000 from the end
    
    
    if(length(baseWgtVar)==0){
      next #we don't have this weight defined in the lookup table
    }
    
    wgtPattern = paste0("^", baseWgtVar, "\\d+$")
    ujkz <- unique(tolower(grep(wgtPattern, fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    ujkz <- ujkz[ujkz != tempVar] #remove the weight value itself from the replicates
    ujkz <- sub(baseWgtVar, "", ujkz, ignore.case = TRUE) #strip away and leave just the numeric variable name ending as a string
    
    if(length(ujkz)>0){
      tmpWgt <- list()
      
      if(grepl("^wt(a|b|c)000$", tempVar, ignore.case = TRUE)){
        tmpWgt[[1]] <- list(jkbase=baseWgtVar, jksuffixes=as.character(ujkz), psuVar="bb9analpsu", stratumVar="bb9analstr")
      }else{
        tmpWgt[[1]] <- list(jkbase=baseWgtVar, jksuffixes=as.character(ujkz), psuVar="bb12analpsu", stratumVar="bb12analstr")
      }
      
      names(tmpWgt)[[1]] <- tempVar
      weights <- c(weights,tmpWgt)
    }
  }
  
  return(weights)
}

#the .csv files for the RUD data are accompanied by two .txt files
#the *_metadata.txt and the *_layout.txt
getMetaFormatDictionary <- function(metaDataFile, formatDataFile){
  
  if(!file.exists(metaDataFile)){
    stop(paste0("No metadata file found at path ", metaDataFile, "."))
  }
  
  if(!file.exists(formatDataFile)){
    stop(paste0("No format layout file found at path ", formatDataFile, "."))
  }
  
  #prepare return dictionary
  dict <- list("variableName" = character(0),
               "Start" = integer(0),
               "End" = integer(0),
               "Width" = integer(0),
               "Decimal" = integer(0),
               "Labels" = character(0),
               "labelValues" = character(0),
               "Type" = character(0),
               "pvWt" = character(0),
               "dataType" = character(0),
               "weights" = character(0))
  
  linesFormat <- readLines(formatDataFile)
  linesMeta <- readLines(metaDataFile)
  
  #omit first line as it just states its metadata
  linesMeta <- linesMeta[-1]
  
  colsFormat <- strsplit(linesFormat, "|", fixed=TRUE) #creates a list object
  colsFormat <- data.frame(t(sapply(colsFormat,c)), stringsAsFactors = FALSE) #convert the list to data.frame
  colnames(colsFormat) <- c("varname", "format", "recIndex")
  colsFormat$varname <- tolower(colsFormat$varname)
  
  colsMeta <- strsplit(linesMeta, "|", fixed=TRUE)
  colsMeta <- data.frame(t(sapply(colsMeta,c)), stringsAsFactors = FALSE)
  colnames(colsMeta) <- c("flag1", "flag2", "varname", "label", "value")
  
  #subset the variable labels from the description labels
  colLbls <- subset(colsMeta, flag1==0)
  colValLbls <- subset(colsMeta, flag1==1)
  
  colsFormat$sortOrd <- 1:nrow(colsFormat)
  colsFormat <- merge(colsFormat, colLbls, by="varname", all.x=TRUE, all.y=FALSE)
  colsFormat <- colsFormat[order(colsFormat$sortOrd), ]
  
  dict$variableName <- colsFormat$varname
  dict$Start <- seq_along(1:nrow(colsFormat)) #keep a sort order of sorts
  dict$End <- rep(NA, nrow(colsFormat))
  dict$Width <- rep(NA, nrow(colsFormat))
  dict$Decimal<- rep(NA, nrow(colsFormat))
  dict$Labels<- colsFormat$label
  dict$labelValues<- rep("", nrow(colsFormat))
  dict$Type<- rep("", nrow(colsFormat))
  dict$pvWt<- rep("", nrow(colsFormat))
  dict$dataType<- rep("", nrow(colsFormat))
  dict$weights<- rep("", nrow(colsFormat))
  
  dict <- data.frame(dict, stringsAsFactors = FALSE)
  
  for(v in dict$variableName){
    lblSubset <- colValLbls[colValLbls$varname==v, ]
    
    keys <- lblSubset$value
    lbls <- lblSubset$label
    
    dict[dict$variableName==v, "labelValues"] <- paste(keys, lbls, collapse = "^", sep = "=")
  }
  
  #now work with the layout file to gather the datatype and the precision
  lines <- readLines(formatDataFile)
  cols <- strsplit(lines, "|", fixed=TRUE) #creates a list object
  cols <- data.frame(t(sapply(cols,c)), stringsAsFactors = FALSE) #convert the list to data.frame
  
  numericVars <- colsFormat$varname[grepl("^F", colsFormat$format)] #get the names of all the numeric variables
  charVars <- colsFormat$varname[!grepl("^F", colsFormat$format)]
  
  dict$dataType[tolower(dict$variableName) %in% tolower(numericVars)] <- "numeric"
  dict$dataType[tolower(dict$variableName) %in% tolower(charVars)] <- "character"
  
  #pull out the decimal places and widths
  dict$Decimal <- as.numeric(ifelse(substr(colsFormat$format,1,1) == "F" & grepl(".", colsFormat$format, fixed = TRUE), sapply(strsplit(colsFormat$format,"\\."), function(x) { tail(x,1) } ), rep(NA, nrow(dict)) ))
  dict$Width <- gsub("[a-zA-Z]","",sapply(strsplit(colsFormat$format,"\\."), function(x) { head(x,1) } ))
  
  #lastly, convert numeric values to integer when they really should be an integer
  dict$dataType[dict$dataType=="numeric" & is.na(dict$Decimal) & dict$Width < 8] <- "integer"
  dict$Decimal[dict$dataType=="integer"] <- 0
  
  return(dict)
}


buildBBDataList_2012 <- function(lafObj, fileFormat){
  
  dataList <- list()
  
  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Data"]] <- dataListItem(lafObject = lafObj,
                                     fileFormat = fileFormat,
                                     levelLabel = "Data",
                                     forceMerge = TRUE,
                                     parentMergeLevels = NULL,
                                     parentMergeVars = NULL,
                                     mergeVars = NULL,
                                     ignoreVars = NULL,
                                     isDimLevel = TRUE)
  
  return(dataList)
}
