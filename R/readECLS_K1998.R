#' @title Connect to ECLS-K 1998 Data
#'
#' @description Opens a connection to an ECLS-K 1998 data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory path(s) to the 
#'             ECLS-K-extracted fixed-with-format (.dat) set of data files
#' @param filename a character value of the name of the fixed-width-file (.dat)
#'                 data file in the specified \code{path} to be read
#' @param layoutFilename a character value of the filename of either the ASCII
#'                       text (.txt) layout file of the \code{filename} within
#'                       the specified \code{path}, 
#'                       OR a character value of the  filename of the SPSS syntax (.sps) layout file of the \code{filename} within the specified \code{path}
#' @param forceReread a logical value to force rereading of all processed data. 
#'                    The default value of \code{FALSE} will speed up the read function by using existing read-in data already processed.
#'                    
#' @param verbose a logical value that will determine if you want verbose output while the \code{readECLS-K1998} function is running to indicate processing progress.
#'                The default value is \code{TRUE}.
#' @details Reads in the unzipped files downloaded from the ECLS-K 1998 longitudinal database(s) to an \code{edsurvey.data.frame}.  The ECLS-K 1998-99 study consisted of
#'          three distinct separate datasets that cannot be combined: (1) Child Grades K-8 Data, (2) School Base-Year Data, and (3) Teacher Base-Year Data.
#'          The \code{filename} and \code{layoutFilename} arguments are defaulted to the corresponding Child K-8 default filenames.
#'
#' 
#' @return
#'  an \code{edsurvey.data.frame} for the ECLS-K 1998 longitudinal dataset
#'
#' @seealso \code{\link{readECLS_K2011}}, \code{\link{readNAEP}}, \code{\link{getData}}, \code{\link{downloadECLS_K}}
#' @author Tom Fink
#'
#' @example \man\examples\readECLS_K1998.R
#' 
#' @export
readECLS_K1998 <- function(path = getwd(),
                           filename = "eclsk_98_99_k8_child_v1_0.dat",
                           layoutFilename = "Layout_k8_child.txt",
                           forceReread = FALSE, verbose = TRUE) {

  #setup file list to work with
  fileList <- list(dataFile=unlist(file.path(path, filename))[1],
                   layoutFile=unlist(file.path(path, layoutFilename))[1])
  
  #validate files::get the filecount to see if we have any missing or excess files
  validateData <- sapply(fileList$dataFile, function(x){
    file.exists(x)
  })
  layoutData <- sapply(fileList$layoutFile, function(x){
    file.exists(x)
  })
  
  if(!all(validateData==TRUE)){
    missingVars <- names(validateData==TRUE)
    if(length(missingVars)>0){
      stop(paste0("Cannot find specified data file ", sQuote(missingVars), " in path ", sQuote(path), "."))
    }
  }
  
  if(!all(layoutData==TRUE)){
    missingVars <- names(layoutData==TRUE)
    
    if(length(missingVars)>0){
      stop(paste0("Cannot find specified layout file ", sQuote(missingVars), " in path ", sQuote(path), "."))
    }
  }
  
  cacheInfo <- list(cacheFilepath = file.path(path, gsub("\\.dat$", "\\.txt", filename, ignore.case = TRUE)),
                    cacheMetaFilepath = file.path(path, gsub("\\.dat$", "\\.meta", filename, ignore.case = TRUE)))
  
  processArgs <- list(files = fileList,
                      cacheFileInfo = cacheInfo,
                      forceReread = forceReread,
                      verbose = verbose)

  retryProc <- tryCatch({processedData <- do.call("processECLS_K1998", processArgs, quote = TRUE)
                          FALSE
                        }, error = function(e){
                          TRUE #flag to retry
                        }, warning = function(w){
                          TRUE #flag to retry
                        })

  if (retryProc){
    processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
    processedData <- tryCatch(do.call("processECLS_K1998", processArgs, quote = TRUE),
                              error = function(e){
                                stop(paste0("Unable to process ECLS_K data. Possible file corruption with source data. ",
                                            "Error message: ", e))
                              })
  }
  
  weights <- buildECLSKWeightList(processedData$fileFormat)
  attr(weights, "default") <- "" #no default weight
  
  dataList <- buildECLSK_dataList(processedData$data, processedData$fileFormat)
  
  pvs <- list() #no plausible values or achievement levels?
  omittedLevels <- c("NOT APPLICABLE", 
                     "DATA SUPPRESSED", 
                     "SUPPRESSED", 
                     "REFUSED", 
                     "DON'T KNOW", 
                     "NOT ASCERTAINED", 
                     NA,
                     "(Missing)")
  
  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = dataList,
                      weights = weights,
                      pvvars = pvs,
                      subject = "Children's Early School Experience",
                      year = "1998-1999",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "K-8 Grade(s)",
                      achievementLevels = NULL,
                      omittedLevels = omittedLevels,
                      survey = "ECLS_K",
                      country = "USA",
                      psuVar = NULL, #psu is specific to each weight variable
                      stratumVar = NULL, #stratum is specific to each weight variable
                      jkSumMultiplier = 1,
                      validateFactorLabels = TRUE) #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
}

processECLS_K1998 <- function (files,
                           cacheFileInfo,
                           forceReread,
                           verbose) {
  
  runProcessing <- TRUE #set default value
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFileInfo$cacheFilepath)){
    if(file.exists(cacheFileInfo$cacheMetaFilepath)){
      
      cacheRDS <- readRDS(cacheFileInfo$cacheMetaFilepath)
      
      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "ECLS_K")){
        runProcessing <- FALSE
        fileFormat <- cacheRDS$fileFormat
      }
    }
  }
  
  #force reprocess if called for
  if(forceReread==TRUE){
    runProcessing <- TRUE
  }
  
  if(runProcessing==TRUE){
    #first delete the existing cache file if it exists in case the processing errors then it won't pickup the cache file
    if(file.exists(cacheFileInfo$cacheMetaFilepath)){
      file.remove(cacheFileInfo$cacheMetaFilepath)
    }
    
    if(grepl("\\.txt$", files$layoutFile, ignore.case=TRUE)){
      if(verbose){
        cat(paste0("Processing text file format file.\n"))
      }
      fileFormat <- parseTEXTFileFormat_NCES(files$layoutFile)
    }else if(grepl("\\.sps$", files$layoutFile, ignore.case=TRUE)){
      if(verbose){
        cat(paste0("Processing SPSS syntax file.\n"))
      }
      fileFormat <- parseSPSSFileFormat(files$layoutFile)
    }else{
      stop(paste0("File layout file must be either an ASCII (.txt) layout file or an SPSS (.sps) syntax file."))
    }
    
    #must open with all columns as character fields first:: For numeric values they use a '.' marker in the FWF datafile so we will need to convert those before converting to numeric
    dataLAF <- laf_open_fwf(files$dataFile, column_types = rep("character", length(fileFormat$variableName)), column_widths = fileFormat$Width, column_names = fileFormat$variableName)
    
    #define chunk size to read the values in:: chunk size should be large enough to accurately detect correct column data types, but small enough to not take up all the memory
    rowChunkSize <- 5000
    maxRows <- nrow(dataLAF)
    
    rowChunks <- split(1:maxRows, ceiling(seq_along(1:maxRows)/rowChunkSize)) #break up the number of rows into our chunk size
    
    for(rci in 1:length(rowChunks)){
      
      if(verbose==TRUE){
        cat(paste0("Processing Data, n columns ", nrow(fileFormat), ", rows ", min(rowChunks[[rci]]), " to ", max(rowChunks[[rci]]), " of ", maxRows, ".\n"))
      }
      
      dataChunk <- dataLAF[rowChunks[[rci]], ] #get the rows of our specific row chunk
      formattedTxt <- matrix(nrow=nrow(dataChunk), ncol=ncol(dataChunk))
      
      for(coli in 1:ncol(dataChunk)){
          xCol <- dataChunk[,coli]
          xCol[xCol=="."] <- NA #remove any null indicators, will be strictly '.' value
          xCol[trimws(xCol, which="both")==""] <- NA
          
          #determine data types as the types are not defined in the ascii file layout on first group
          #no need to change FWF widths based on this since the original .dat file widths as adequate size
          if(rci==1){
            if(suppressWarnings(all(!is.na(as.numeric(xCol[!is.na(xCol)]))))){ #determine if all the NA values are numeric or character
              zCol <- xCol[!is.na(xCol)]
              hasDec <- grepl(".", zCol, fixed = TRUE)
              if(!any(hasDec)){
                precision <- 0
              }else{
                decPos <- regexpr(".", zCol[hasDec], fixed = TRUE)
                precision <- nchar(substring(zCol, decPos+1))
              }
              scale <- nchar(sub(".", "", zCol, fixed = TRUE))
  
              if(max(scale)<8 && max(precision)==0){
                fileFormat$dataType[coli] <- "integer"
                fileFormat$Decimal[coli] <- 0
              }else{
                fileFormat$dataType[coli] <- "numeric"
                fileFormat$Decimal[coli] <- max(as.numeric(precision))
              }
            }else{
              fileFormat$dataType[coli] <- "character"
              fileFormat$Decimal[coli] <- NA
            }
          }
          
          if(fileFormat$dataType[coli] %in% c("numeric") && fileFormat$Decimal[coli]>0){
            multiplier <- 10^as.numeric(fileFormat$Decimal[coli])
            xCol <- as.numeric(xCol) * multiplier
            xColChar <- format(xCol, scientific = FALSE)
            xColChar[is.na(xCol)] <- " "
            
            #test if the multiplier expanded the width beyond the intial set width otherwise FWF spacing issues will pop up
            if(any(nchar(xColChar)>fileFormat$Width[coli])){
              fileFormat$Width[coli] <- max(nchar(xColChar))
              
              #recalibrate the start/end positions for user
              fileFormat$Start <-  c(1,1 + cumsum(fileFormat$Width))[1:length(fileFormat$Width)]
              fileFormat$End <- cumsum(fileFormat$Width)
            }
            
            xCol <- xColChar #swap back names
            xColChar <- NULL
          }
          
          xCol[is.na(xCol)] <- " "
          formattedTxt[,coli] <- format(xCol, scientific = FALSE, width = fileFormat$Width[coli], justify = "right") #store formatted column into matrix for writing
      }
      
      #remove the file if it exists and we are reprocessing
      if(rci==1 && file.exists(cacheFileInfo$cacheFilepath)){
        file.remove(cacheFileInfo$cacheFilepath)
      }
      
      if(verbose==TRUE){
        cat(paste0("Processing data, writing data chunk to disk.\n"))
      }
      #write the fwf formatted matrix
      a <- sapply(1:nrow(formattedTxt), function(rowi){
        cat(paste(formattedTxt[rowi,], collapse=""), file=cacheFileInfo$cacheFilepath, append = TRUE)
        cat(paste("\n"), file=cacheFileInfo$cacheFilepath, append = TRUE)
      })
      
      #minimize memory footprint
      a <- NULL
      dataChunk <- NULL
      formattedTxt <- NULL
    }
    
    #close the existing LAF connection to the .dat file and pickup new LaF handle for the FWF .txt file we just wrote
    LaF::close(dataLAF)
    
    #parse weight variables for the fileFormat
    fileFormat <- identifyECLSKWeights(fileFormat)
    
    #write cache file and .meta
    cacheFile <- list(ver=ifelse(any(search() %in% "EdSurvey"), packageVersion("EdSurvey"), "Invalid"),
                      cacheFileVer=1,
                      ts=Sys.time(),
                      fileFormat=fileFormat)
    
    saveRDS(cacheFile, cacheFileInfo$cacheMetaFilepath)
    
  }else{ #if(runProcessing==TRUE)
    
    if(verbose==TRUE){
      cat(paste0("Found cached data for file ", dQuote(files$dataFile),".\n"))
    }
  }#end if(runProcessing==TRUE) 
  
  dataLAF <- laf_open_fwf(cacheFileInfo$cacheFilepath, column_types = fileFormat$dataType, column_widths = fileFormat$Width, column_names = fileFormat$variableName)
  
  #do caching and testing
  return(list(data = dataLAF,
              fileFormat = fileFormat))
}

#reads an SPSS (.sps) snytax file and prepares the fileformat
parseSPSSFileFormat <- function (inSPSSyntax){
    
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
                     "weights" = character(0),
                     "RecordIndex" = character(0))
  
    # Read in spss control files
    con <- file(inSPSSyntax, open="r")
    controlFile <- readLines(con)
    close.connection(con)
    
    #prep for processing
    controlFile <- gsub("[^[:print:]]","", controlFile) #remove unprintable characters
    controlFile <- trimws(controlFile,which = "both") #remove leading or ending whitespace
    controlFile <- controlFile[controlFile != ""] #remove blank rows
    
    
    # Note: the following lines need to be in order
    # Some syntax files uses single quote instead of double quote
    # Because later code use quote as a pattern to get labelValues, it's necessary
    # to replace relevant single quotes with double quotes
    controlFile <- gsub(" \'"," \"", controlFile) #replace single quote with double quote for later use
    controlFile <- gsub("\' ","\" ", controlFile)
    controlFile <- gsub("^\'|\'$","\"", controlFile)
    controlFile <- gsub("\'/","\"/", controlFile)
    controlFile <- gsub("\'\\.","\"\\.", controlFile)
    
    # GET VARIABLE LIST, FWF POSITIONS, and DATA TYPE
    i <- 1
    while (!grepl("file handle fhand.*name=.*", controlFile[i], ignore.case = TRUE)) { #find file handle function in SPSS file
      i <- i+1
    }
    while (substr(controlFile[i],1,1)!="/") {#find start line of variables by '1-# (1 is first column in .dat file)
      i <- i+1
    }
    
    
    #find where we want to end the variable list chunk
    j <- i+1
    while (controlFile[j]!=".") {#find start line of variables by '1-# (1 is first column in .dat file)
      j <- j+1
    }
    
    lineChunk <- controlFile[i:j]
    lineChunkRecIndex <- which(lineChunk %in% lineChunk[substr(lineChunk,1,1)=="/"]) #record index are defined such as '/2', '/3', etc.  we want to keep these positions as the .dat has multiple rows for one piece of data
    names(lineChunkRecIndex) <- 1:length(lineChunkRecIndex)
    
    lineChunk <- lineChunk[trimws(lineChunk, which="both")!=""]
    lineChunk <- lineChunk[substr(lineChunk,1,1)!="."] #remove unneeded rows
    
    recIndex <- c()
    ri <- 0 #will change to one when first line is noted
    for(lc in lineChunk){
      if (substr(lc,1,1)=="/"){
        ri <- ri + 1
      }else{
        recIndex <- c(recIndex, ri)
      }
    }
    lineChunk <- lineChunk[substr(lineChunk,1,1)!="/"] #remove the table indexes now that we don't need them
    
    xMatch <- regexpr("^\\w+", lineChunk) #get first word
    varName <- tolower(trimws(regmatches(lineChunk, xMatch), which="both")) #use lower case variable names
    names(lineChunk) <- varName #store the variable name here for later use
    
    xMatch <- regexpr(" \\d+-\\d+", lineChunk) #get digits of fwf ###-###
    pos <- trimws(regmatches(lineChunk, xMatch), which="both")
    
    xMatch <- regexpr("\\d+-", pos) #get digits before '-' char
    posStart <- trimws(gsub("-", "", regmatches(pos, xMatch)), which="both")
    posStart <- as.integer(posStart)
    
    xMatch <- regexpr("-\\d+", pos)  #get digits after '-' char           
    posEnd <- trimws(gsub("-", "", regmatches(pos, xMatch)), which="both")
    posEnd <- as.integer(posEnd)
    
    
    #grab extra info contained in parens()
    xMatch <- regexpr("[(].*[)]", lineChunk)
    extraInfo <- toupper(trimws(regmatches(lineChunk,xMatch), which="both"))
    
    #setup default data type and decimal
    xType <- rep("numeric", times=length(varName))
    names(xType) <- varName
    xDec <- rep(0, times=length(varName))
    names(xDec) <- varName
    
    #change character type
    xType[names(xType) %in% names(extraInfo[extraInfo=="(A)"])] <- "character"
    xDec[names(xDec) %in% names(extraInfo[extraInfo=="(A)"])] <- NA
    
    #change the numeric type
    xType[names(xType) %in% names(extraInfo[grepl("\\d+", extraInfo, ignore.case=TRUE)])] <- "numeric"
    xMatch <- regexpr("\\d+", extraInfo)
    xDec[names(xDec) %in% names(extraInfo[grepl("\\d+", extraInfo, ignore.case=TRUE)])] <- as.integer(regmatches(extraInfo, xMatch))
    
    #update the dictionary
    dict$variableName <- varName
    dict$Start <- posStart
    dict$End <- posEnd
    dict$Width <- (dict$End - dict$Start) + 1
    dict$Decimal <- xDec
    dict$dataType <- xType
    dict$RecordIndex <- recIndex
    #########################
    
    #Get the Variable Label section here in chunk
    i <- j #move one row down past the 'VARIABLE LABEL' line
    while (!grepl("variable label", controlFile[i], ignore.case = TRUE)) { #get out next chunk of data (VARIABLE DATA)
      i <- i + 1
    }
    
    j <- i + 1 
    while (!grepl("value labels", controlFile[j], ignore.case = TRUE)) { #get out next chunk of data (VARIABLE DATA)
      j <- j + 1
    }
    
    lineChunk <- controlFile[(i+1):(j-1)] #get all the variable labels as on chunk
    lineChunk <- lineChunk[length(trimws(lineChunk, which = "both"))>0 & lineChunk!="."] #remove any junk lines
    
    #get variable name
    xMatch <- regexpr("^\\w+", lineChunk) #get first word
    varName <- trimws(regmatches(lineChunk, xMatch), which="both")
    
    xMatch <- regexpr("\\s[\"].*[\"]", lineChunk) #find the text between the double quotes in the string
    varLabel <- trimws(regmatches(lineChunk, xMatch), which="both") 
    varLabel <- substr(varLabel, 2, nchar(varLabel)-1) #remove the beginning and end double quotes
    
    #setup the labels
    dict$Labels <- rep("", length(dict$variableName))
    dict$Labels[tolower(dict$variableName)==tolower(varName)] <- varLabel
      
    #prep for gathering the value labels::should look into doing this in one chunk as well, but difficult with the varnames being used as the markers
    dict$labelValues <- rep("", times = length(dict$variableName)) #so we don't have NA values in the labels and it's of proper length
    
    i <- j + 1
    j <- i + 1 #skip the 'VALUE LABELS' row
    varLbls <- list() #prep vars for getting the labels
    varName <- ""
    
    while (controlFile[i]!=".") { #the '.' will signal the end of the section
      tempStr <- trimws(controlFile[i], which = "both")
      
      if(substr(tempStr,1,1)=="/"){ #indicates beginning of definition
        
        #found new var, write the labels to the list if we have them
        if (sum(nchar(varName))>0 && sum(nchar(varLblList[["Value"]]))>0){
          dict$labelValues[tolower(dict$variableName)==tolower(varName)] <- paste(varLblList[["Value"]], varLblList[["Label"]], sep="=", collapse="^") #create the proper labelValue type of string '1=One^2=Two'
        }
        
        #get the new variable name without the "/" char
        varName <- tolower(trimws(gsub("/", "", tempStr, fixed=TRUE), which="both")) #ensure it's lower case to match
        
        #do any prep for parsing labels
        varLblList <- list()
      }else{
        if(substr(tempStr,1,1)=="\""){ #check if first character is a double-quote as the value will be in double-quotes in addition to the varlabel
          xMatch <- regexpr("^[\"].*[\"]\\s", tempStr) #find first quoted item with space after
        }else{
          xMatch <- regexpr("^[^\"]*", tempStr) #find all characters before first double-quote char
        }
        
        varValue <- trimws(regmatches(tempStr, xMatch), which="both")
        
        #check if value in quotes::remove if so
        if(substr(varValue,1,1)=="\"" && substr(varValue,nchar(varValue), nchar(varValue))=="\""){
          varValue <- substr(varValue, 2, nchar(varValue)-1)
        }
        
        #test if a numeric range such as '1 - 20' or '1 - 10' is specified
        #these ranges will be converted to NA for removal
        #as they seem to be indicating a valid range of values instead of specific variable labels
        varValue[grepl("//d* - //d*", varValue, ignore.case = TRUE)] <- NA
        
        if(!is.na(varValue)){
          #get the text label
          xMatch <- regexpr("\\s[\"].*[\"]$", tempStr) #find the text between the double quotes at the end of the string
          varLabel <- trimws(regmatches(tempStr, xMatch), which="both") 
          varLabel <- substr(varLabel, 2, nchar(varLabel)-1) #remove the beginning and end double quotes
          
          #fix any label char values that don't display correctly.
          #The actual character is a 'replacement char' (\uFFFD) but it's raw is converted to three seperate chars: (\u00EF) (\u00BF) (\u00BD)
          varLabel <- gsub("\u00EF\u00BF\u00BD", "'", varLabel, ignore.case = TRUE)
          varLabel <- gsub("\u00E2", "'", varLabel, ignore.case = TRUE) #converts an latin small 'a' with circumflex (u00E2) to apostraphe (u0027)
          
          #get a list of the value and lables
          varLblList[["Value"]] <- c(varLblList[["Value"]], varValue)
          varLblList[["Label"]] <- c(varLblList[["Label"]], varLabel)
        }
      }
      
      i <- i + 1
    }
    
    #add the last variable labels if applicable
    if (nchar(varName)>0 && length(varLblList[["Value"]])>0){
      dict$labelValues[dict$variableName==varName] <- paste(varLblList[["Value"]], varLblList[["Label"]], sep="=", collapse="^") #create the proper labelValue type of string '1=One^2=Two'
    }
    
    #need to populate the Type, pvWt and weights dict sublists so we can convert the list to a data.frame
    dict$Type <- rep("", times = length(dict$variableName))
    dict$pvWt <- rep("", times = length(dict$variableName))
    dict$weights <- rep(FALSE, times = length(dict$variableName))
    
    #need to update the position start/end variables as the positions are thrown off by having SPSS tables defined::tables will be removed
    dict$Start <-  c(1,1 + cumsum(dict$Width))[1:length(dict$Width)]
    dict$End <- cumsum(dict$Width)
    
    return(data.frame(dict, stringsAsFactors = FALSE))
}

#the ascii text meta files seem to have inconsistancy between even NCES longitudinal data and must be specific for each individual study
parseTEXTFileFormat_NCES <- function (inTxtFile){
  
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
  
  # Read in spss control files
  con <- file(inTxtFile)
  controlFile <- readLines(con)
  close.connection(con)
  
  #prep for processing
  controlFile <- gsub("[^[:print:]]","", controlFile) #remove unprintable characters
  controlFile <- trimws(controlFile,which = "both") #remove leading or ending whitespace
  controlFile <- controlFile[controlFile != ""] #remove blank rows
  
  # GET VARIABLE LIST, FWF POSITIONS, and DATA TYPE
  i <- 1
  
  while (controlFile[i]!="/* ASCII Dataset File Name */") { #find file handle function in SPSS file
    i <- i+1
  }
  i <- i+1
  datasetName <- trimws(controlFile[i], which = "both")
  
  while (controlFile[i]!="/* Total Record Length */") { #find file handle function in SPSS file
    i <- i+1
  }
  i <- i+1
  maxRecordLen <- as.numeric(trimws(controlFile[i], which = "both"))
  
  while (controlFile[i]!="/* Variable Names, Locations, and Descriptions */") { #find file handle function in SPSS file
    i <- i+1
  }
  i <- i+1 #first row starts below the header
  
  #find where we want to end the variable list chunk
  j <- i+1
  while (controlFile[j]!="/* Variable Value Labels */") {#find start line of variables by '1-# (1 is first column in .dat file)
    j <- j+1
  }
  
  lineChunk <- controlFile[i:(j-1)]
  
  varMatch <- regexpr("^\\w+\\s{0,}\\d+-\\d+", lineChunk) #get first word in line and include the position chars as sometimes they are right next to each other with no space
  varName <- tolower(regmatches(lineChunk, varMatch)) #use lower case for the names
  varName <- trimws(gsub("\\d+-\\d+","", varName), which = "both") #remove the position indicator  ###-###
  names(varName) <- varName #store the variable name here for later use
  
  posMatch <- regexpr("\\d+-\\d+", lineChunk) #finds the digits and the dash with at least two spaces on either side
  pos <- trimws(regmatches(lineChunk, posMatch), which="both")
  
  posStartMatch <- regexpr("\\d+-", pos) #get digits before '-' char
  posStart <- trimws(gsub("-", "", regmatches(pos, posStartMatch)), which="both")
  posStart <- as.integer(posStart)
  
  posEndMatch <- regexpr("-\\d+", pos)  #get digits after '-' char           
  posEnd <- trimws(gsub("-", "", regmatches(pos, posEndMatch)), which="both")
  posEnd <- as.integer(posEnd)
  
  #remove the first occurance of the var name as well as the position indicators
  descMatch <- regexpr("\\d+-\\d+.*$", lineChunk)
  varDesc <- regmatches(lineChunk, descMatch) #grab matches, but still need to remove position
  varDesc <- gsub("^\\d+-\\d+","", varDesc) #remove position indicator
  
  #trim
  varDesc <- trimws(varDesc, which="both")
  
  #in special cases the variable name is adjacent to the start-end positions
  #we need to test for these and clean them up otherwise it will hard crash LaF and R
  #Example: F3ICREDDBLMAJ_1130-132 -->  The true var name is F3ICREDDBLMAJ_1 and the start is position 130
  #outOfRangeIndex <- which(posStart > maxRecordLen, arr.ind = TRUE)
  outOfRangeIndex <- which(posStart > posEnd, arr.ind = TRUE)
  
  for(ri in outOfRangeIndex){
    xChr <- substr(posStart[ri],1,1)
    varName[ri] <- paste0(varName[ri], xChr)
    names(varName)[ri] <- varName[ri] #update name to be consistent
    posStart[ri] <- as.numeric(substr(posStart[ri], 2, nchar(posStart[ri])))
  }
  
  #check for any variable name duplication here
  #duplicated names can occur when the variable name is too long for the file layout specification
  #we need to assume that the variable is numbered: '1', '2', '3', etc.
  if(anyDuplicated(varName)>0){
    dupeNames <- unique(varName[duplicated(varName)]) #gather the specific duplicated variable names
    for(dN in dupeNames){
      dupeIndex <- which(varName %in% dN, arr.ind = TRUE) #find which index the dupes are located in the overall vector 
      
      #number the duplicates(1, 2, 3, etc.) in the order they exist in the original vector
      for(i in 1:length(dupeIndex)){
        varName[dupeIndex[i]] <- paste0(dN, i)
      }
    }
  }
  
  #for some reason even between the fwf datasets the ASCII layout files have slight variations between their column specs
  #no need to offset if the end positions equal the 
  if(posEnd[1]==posStart[2]){
    fwfWidthOffset <- 0
  }else{
    fwfWidthOffset <- 1
  }
  
  #update the dictionary
  dict$variableName <- varName
  dict$Start <- posStart
  dict$End <- posEnd
  dict$Width <- (dict$End - dict$Start) + fwfWidthOffset
  dict$Decimal <- rep("", length(varName))
  dict$Labels <- varDesc
  dict$labelValues <- rep("", length(varName))
  dict$Type <- rep("", times = length(dict$variableName))
  dict$pvWt <- rep("", times = length(dict$variableName))
  dict$dataType <- rep("", times = length(dict$variableName))
  dict$weights <- rep(FALSE, times = length(dict$variableName))
  #########################
  
  #Get the Variable Label section here in chunk
  i <- j+1 #move one row down past the '/* Variable Value Labels */' line to the end of the file
  
  lineChunk <- controlFile[i:length(controlFile)] #get all the variable labels as on chunk
  
  #some variable names were trimmed in the layout specs
  #identify which variables have no labels and compare
  noLabelVars <- lineChunk[!grepl("[0-9].*[=].*", lineChunk, ignore.case = TRUE)]
  noLabelVars <- noLabelVars[!(tolower(noLabelVars) %in% tolower(dict$variableName))] #omit any vars we already can match on to limit our pool
  
  for(v in noLabelVars){
      if(any(substr(tolower(v),1,15)==tolower(dict$variableName))){ #NCES text file layouts only have space for 15 characters, rest of the chars are cutoff
        dict$variableName[tolower(dict$variableName)==substr(tolower(v),1,15)] <- paste0(tolower(v))
      }
  }
  
  
  #get variable row indexes
  subChunk <- which(tolower(lineChunk) %in% tolower(dict$variableName), arr.ind = TRUE)
  
  if(length(subChunk)>0){
    for(varIndex in 1:length(subChunk)){
      tempVarName <- tolower(lineChunk[subChunk[varIndex]])
      
      nextChunkIndex <- subChunk[varIndex + 1]
      if(is.na(nextChunkIndex)){
        nextChunkIndex <- length(lineChunk)
      }else{
        nextChunkIndex <- nextChunkIndex - 1 #we want lines between vars
      }
      
      lblLines <- lineChunk[(subChunk[(varIndex)]+1):nextChunkIndex]
      splitPos <- regexpr("=", lblLines)
      
      #get everything before the first '=' symbol
      lblVals <- trimws(substr(lblLines, 1, (splitPos-1)), which="both")
      
      lblTxt <- trimws(substring(lblLines, (splitPos+1)), which = "both")
      lblTxt <- substr(lblTxt, 2, (nchar(lblTxt)-1)) #remove leading/ending quotes
      
      valsForCleanup <- grepl(" - ", lblVals, fixed=TRUE) #these define a sort of value range but are not true value labels
      
      valsToAdd <- c()
      lblsToAdd <- c()
      
      lblVals <- c(valsToAdd, lblVals[!valsForCleanup])
      lblTxt <- c(lblsToAdd, lblTxt[!valsForCleanup])
      
      #fix any label char values that don't display correctly.
      #The actual character is a 'replacement char' (\uFFFD) but it's raw is converted to three seperate chars: (\u00EF) (\u00BF) (\u00BD)
      lblTxt <- gsub("\u00EF\u00BF\u00BD", "'", lblTxt, ignore.case = TRUE)
      lblTxt <- gsub("\u00E2", "'", lblTxt, ignore.case = TRUE) #converts an latin small 'a' with circumflex (u00E2) to apostraphe (u0027)
      
      dict$labelValues[dict$variableName==tempVarName] <- paste(lblVals, lblTxt, sep="=", collapse="^")
    }
  }#end if(length(subChunk)>0)
  
  #ensure rows are sorted before returning so the start/width positon is correct
  finalFormat <- data.frame(dict, stringsAsFactors = FALSE)
  finalFormat <- finalFormat[order(finalFormat$Start), ]
  
  #check for any gaps in the start/end positions of the defined fileformat, LaF cannot support missing gaps as it relies on width value
  finalFormat <- validateFWF_FileFormat(finalFormat)
  
  return(finalFormat)
}

#returns a validated fwf file format
#1) checks for any missing numeric gaps between the start and end postions and inserts 'xGAP' fields for LaF to operate properly
validateFWF_FileFormat <- function(fileFormat){
  
  startPos <- min(fileFormat$Start)
  endPos <- max(fileFormat$End)
  
  if(startPos!=1){
    stop("Improper file format starting position found.  All fixed-with-format files should start with a numerical value of '1'")
  }
  
  posIndex <- data.frame(index=startPos:endPos)
  posIndex$chkFlag <- FALSE #default all check flags to false initially
  
  #ensure file format is ordered correctly
  fileFormat <- fileFormat[order(fileFormat$Start), ]
  
  for(xVar in fileFormat$variableName){
    xStart <- fileFormat[fileFormat$variableName==xVar, "Start"]
    xEnd <- fileFormat[fileFormat$variableName==xVar, "End"]
    
    for(idx in xStart:xEnd){
      posIndex[posIndex$index==idx, "chkFlag"] <- TRUE #has a value
    }
  }
  
  posIndex <- posIndex[posIndex$chkFlag==FALSE, ]
  
  if(nrow(posIndex)>0){
    
    for(idx in posIndex$index){
      
      rnames <- row.names(fileFormat)
      ri <- nrow(fileFormat) + 1
      
      fileFormat[ri, "variableName"] <- paste0("x___fileformatgap", idx)
      fileFormat[ri, "Start"] <- idx
      fileFormat[ri, "End"] <- idx
      fileFormat[ri, "Width"] <- 1
      fileFormat[ri, "Decimal"] <- NA
      fileFormat[ri, "Labels"] <- paste0("Gap in File Format Definition. Column Position: ", idx)
      fileFormat[ri, "labelValues"] <- ""
      fileFormat[ri, "Type"] <- ""
      fileFormat[ri, "pvWt"] <- ""
      fileFormat[ri, "dataType"] <- "character"
      fileFormat[ri, "weights"] <- FALSE
      
      #ensure row.names are properly applied
      row.names(fileFormat) <- c(rnames, paste0("x___fileformatgap", idx))
    }
    
    #reorder before returning
    fileFormat <- fileFormat[order(fileFormat$Start), ]
  }
  
  return(fileFormat)
}

#identified the ECLS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyECLSKWeights <- function(fileFormat){
  
  varNames <- fileFormat$variableName
  
  #BY= BaseYear; S=School; B=Teacher; C=Child
  wgtVars <- grep("^(S|B|C).*(W|C|P|S|M|E|R)0$", varNames, value=TRUE, ignore.case = TRUE)
  wgtVarsSpecial <- grep("Y2COMW0", varNames, value=TRUE, ignore.case = TRUE)
  wgtVarsSpecial <- grep("BYCOMW0", varNames, value=TRUE, ignore.case = TRUE)
  wgtVars <- c(wgtVars, wgtVarsSpecial)

  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- fileFormat$variableName %in% wgtVars
  
  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildECLSKWeightList <- function(fileFormat){
  
  wgtVars <- fileFormat[fileFormat$weights==TRUE, "variableName"]
  varNames <- fileFormat$variableName
  
  #no wgts found
  if(length(wgtVars)==0){
    return(NULL)
  }
  
  weights <- list()
  
  for(i in 1:length(wgtVars)){
    tempVar <- wgtVars[i]
    testJKprefix <- substr(tempVar, 1, nchar(tempVar)-1) #strip the ending '0' from the variable::all the replicates will have the same name but numbered 1-n 
    testJKprefix2 <- substr(tempVar, 1, nchar(tempVar)-2) #strip the ending '[w]0' from the variable
    
    ujkz <- unique(tolower(grep(paste0("^","(", testJKprefix ,")","[1-9]"), fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    ujkz <- gsub(tolower(testJKprefix), "", ujkz, fixed = TRUE) #remove jk to leave the numeric values
    
    #gather PSU an Stratum info, For 1998 there isn't much consistancy for the naming conventions
    #between the weight variable name and it's associated PSU and Stratum variable names
    if(tolower(tempVar)=="c4_7cw0"){
      psuVar <- grep("^c47fcpsu$", varNames, ignore.case = TRUE, value = TRUE)
      strVar <- grep("^c47fcstr$", varNames, ignore.case = TRUE, value = TRUE)
    }else if(tolower(tempVar)=="c4_7pw0"){
      psuVar <- grep("^c47fppsu$", varNames, ignore.case = TRUE, value = TRUE)
      strVar <- grep("^c47fpstr$", varNames, ignore.case = TRUE, value = TRUE)
    }else{
      #gather the psu variable, rules vary widely between the original weight variable name
      psuVar <- c(grep(paste0(tempVar, "psu$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "psu$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "ps$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(gsub("_", "", testJKprefix, fixed = TRUE), "psu$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix2, "psu$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(substr(testJKprefix,1,2), "t", substr(testJKprefix,3,nchar(testJKprefix)),"psu$"), varNames, ignore.case = TRUE, value = TRUE))
      
      #gather the stratum variable, rules vary widely between the original weight variable name
      strVar <- c(grep(paste0(tempVar, "str$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "str$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix, "st$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(gsub("_", "", testJKprefix, fixed = TRUE), "str$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(testJKprefix2, "str$"), varNames, ignore.case = TRUE, value = TRUE),
                  grep(paste0(substr(testJKprefix,1,2), "t", substr(testJKprefix,3,nchar(testJKprefix)),"str$"), varNames, ignore.case = TRUE, value = TRUE))
    }
    psuVar <- unique(psuVar)
    strVar <- unique(strVar)
    
    if(length(psuVar)!=1){
      stop(paste0("Cannot Find Primary Sampling Unit Variable for Weight: ", tempVar ))
    }
    
    if(length(psuVar)!=1){
      stop(paste0("Cannot Find Stratum Variable for Weight: ", tempVar ))
    }
    
    if(length(ujkz)>0){
        tmpWgt <- list()
        tmpWgt[[1]] <- list(jkbase=testJKprefix, jksuffixes=as.character(ujkz), psuVar=psuVar, stratumVar=strVar)
        names(tmpWgt)[[1]] <- tempVar
        weights <- c(weights,tmpWgt)
    }
  }
  
  return(weights)
}

#builds the ECLS_K dataList object
buildECLSK_dataList <- function(LaF, FF){
  
  dataList <- list()
  
  dataList[["Data"]] <- dataListItem(lafObject = LaF,
                                     fileFormat = FF,
                                     levelLabel = "Data",
                                     forceMerge = TRUE,
                                     parentMergeLevels = NULL,
                                     parentMergeVars = NULL,
                                     mergeVars = NULL,
                                     ignoreVars = NULL,
                                     isDimLevel = TRUE)
  
  return(dataList)
}


