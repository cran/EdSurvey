#' @title Connect to ECLS--K 2011 Data
#'
#' @description Opens a connection to an ECLS--K 2011 data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character value to the full directory path(s) to the ECLS--K 2010--11 extracted fixed-with-format (.dat) set of data files
#' @param filename a character value of the name of the fixed-width (.dat) data file in the specified \code{path} to be read
#' @param layoutFilename a character value of the filename of either the ASCII (.txt) layout file of the \code{filename} within the specified \code{path}
#'                       or a character value of the  filename of the SPSS syntax (.sps) layout file of the \code{filename} within the specified \code{path}
#' @param forceReread a logical value to force rereading of all processed data.
#'                    The default value of \code{FALSE} will speed up the read function by using existing read-in data already processed.
#'
#' @param verbose a logical value that will determine if you want verbose output while the \code{readECLS--K2011} function is running to indicate processing progress.
#'                The default value is \code{TRUE}.
#' @details Reads in the unzipped files downloaded from the ECLS--K 2010--11 longitudinal dataset.
#'
#'
#' @return an \code{edsurvey.data.frame} for the ECLS--K 2010--11 longitudinal dataset
#'
#' @seealso \code{\link{readECLS_K1998}}, \code{\link{readNAEP}}, \code{\link{getData}}, and \code{\link{downloadECLS_K}}
#' @author Tom Fink
#' @example man/examples/readECLS_K2011.R
#' @export
readECLS_K2011 <- function(path = getwd(),
                           filename = "childK4p.dat",
                           layoutFilename = "ECLSK2011_K4PUF.sps",
                           forceReread = FALSE,
                           verbose = TRUE) {

  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  path <- suppressWarnings(normalizePath(unique(path), winslash = "/"))
  
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

  retryProc <- tryCatch({processedData <- do.call("processECLS_K2011", processArgs, quote = TRUE)
  FALSE
  }, error = function(e){
    TRUE #flag to retry
  }, warning = function(w){
    TRUE #flag to retry
  })

  if (retryProc){
    processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
    processedData <- tryCatch(do.call("processECLS_K2011", processArgs, quote = TRUE),
                              error = function(e){
                                stop(paste0("Unable to process ECLS_K 2011 data. Possible file corruption with source data. ",
                                            "Error message: ", e))
                              })
  }

  weights <- buildECLSK2011WeightList(processedData$fileFormat)
  attr(weights, "default") <- "" #no default weight

  pvs <- list() #no plausible values or achievement levels?
  omittedLevels <- c("-1: NOT APPLICABLE",
                     "-2: DATA SUPPRESSED",
                     "-4: DATA SUPPRESSED, INSUFFICIENT PRACTICE DUE TO PROGRAMMING ERROR",
                     "-4: DATA SUPPRESSED FOR ADMINISTRATION ERROR",
                     "-5: ABBREVIATED SURVEY (ITEM NOT FIELDED)",
                     "-7: REFUSED",
                     "-8: DON'T KNOW",
                     "-9: NOT ASCERTAINED",
                     NA,
                     "(Missing)")

  edsurvey.data.frame(userConditions = list(),
                      defaultConditions = NULL,
                      dataList = buildECLS_K2011_dataList(processedData$data, processedData$fileFormat),
                      weights = weights,
                      pvvars = pvs,
                      subject = "Children's Early School Experience",
                      year = "2010-2011",
                      assessmentCode = "Longitudinal",
                      dataType = "Longitudinal Data",
                      gradeLevel = "K-4 Grade(s)",
                      achievementLevels = NULL, #no achievement levels
                      omittedLevels = omittedLevels,
                      survey = "ECLS_K2011",
                      country = "USA",
                      psuVar = NULL, #psu is specific to each weight variable
                      stratumVar = NULL, #stratum is specific to each weight variable
                      jkSumMultiplier = 1,
                      validateFactorLabels = TRUE) #the validateFactorLabels will check in `getData` if all values have a defined label, any missing labels will be automatically added.
}

processECLS_K2011 <- function (files,
                           cacheFileInfo,
                           forceReread,
                           verbose) {

  runProcessing <- TRUE #set default value
  #check and validate any cached files to see if they should be used
  if(file.exists(cacheFileInfo$cacheFilepath)){
    if(file.exists(cacheFileInfo$cacheMetaFilepath)){

      cacheRDS <- readRDS(cacheFileInfo$cacheMetaFilepath)

      if(!cacheMetaReqUpdate(cacheRDS$cacheFileVer, "ECLS_K2011")){
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
    if(file.exists(cacheFileInfo$cacheFilepath)){
      file.remove(cacheFileInfo$cacheFilepath)
    }

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

    #record index is for multi-lined .dat file processing
    #find max recordindex if SPSS syntax, or set to value of 1 otherwise
    if(is.null(fileFormat$RecordIndex) || max(fileFormat$RecordIndex)==1){
      maxRecordIndex = 1 #no need to do any additional processing
      tempFilename <- NULL
    }else{

      if(verbose==TRUE){
        cat("Flattening multi-line *.dat file to temp file.\n")
      }

      maxRecordIndex <- max(fileFormat$RecordIndex) #no NA's should be present here
      tempFilename <- gsub("\\.dat$", ".tmp", files$dataFile, ignore.case = TRUE)

      if(file.exists(tempFilename)){
        file.remove(tempFilename)
      }

      readConnection <- file(files$dataFile, "r")
      writeConnection <- file(tempFilename, "w")

      while(TRUE){
        linePart <- readLines(readConnection, maxRecordIndex)
        #be sure to exit once done
        if(length(linePart)==0){
          break
        }

        writeStr <- paste0(linePart, collapse = "")
        writeLines(writeStr, writeConnection)
      }

      close(readConnection)
      close(writeConnection)

      #prep now to read the .tmp data file
      files$dataFile <- tempFilename
    }

    dataLAF <- laf_open_fwf(files$dataFile, column_types = rep("character", length(fileFormat$variableName)), column_widths = fileFormat$Width, column_names = fileFormat$variableName)

    #define chunk size to read the values in:: chunk size should be large enough to accurately detect correct column data types, but small enough to not take up all the memory
    maxRows <- nrow(dataLAF)
    rowChunkSize <- 5000

    rowChunks <- split(1:maxRows, ceiling(seq_along(1:maxRows)/rowChunkSize)) #break up the number of rows into our chunk size

    for(rci in 1:length(rowChunks)){

      if(verbose==TRUE){
        cat(paste0("Processing data, number of columns ", nrow(fileFormat), ", rows ", min(rowChunks[[rci]]), " to ", max(rowChunks[[rci]]), " of ", maxRows, ".\n"))
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

        xCol[is.na(xCol)] <- " " #convert to blank for writing to FWF
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
    close(dataLAF)

    if(grepl("\\.tmp$", files$dataFile, ignore.case = TRUE)){
      file.remove(files$dataFile)
    }

    #parse weight variables for the fileFormat
    fileFormat <- identifyECLSK2011Weights(fileFormat)

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

  #return LAF as open to edsurvey.data.frame constructor where it needs it open to first build, then it handles closing within there
  dataLAF <- laf_open_fwf(cacheFileInfo$cacheFilepath, column_types = fileFormat$dataType, column_widths = fileFormat$Width, column_names = fileFormat$variableName)

  #do caching and testing
  return(list(data = dataLAF,
              fileFormat = fileFormat))
}

#identified the ECLS weights based on the file format data.frame and marks them as weights TRUE/FALSE in the fileFormat
identifyECLSK2011Weights <- function(fileFormat){

  varNames <- fileFormat$variableName

  #unable to grepl as it picked up too many replicate weights
  wgtVars <- c("W1C0", "W1A0", "W1T0", "W1P0",
               "W2P0", "W12P0", "W1_2P0", "W12T0", "W12AC0", "W1PZ0", "W12PZ0",
               "W2SCH0", "W2C_2P_2TZ0",
               "W3CF3P_30", "W3CF3P3T0", "W4PF40", "W4CF4P_20", "W4CF4P20", "W4CF4P40", "W4CF4P4T0",
               "W4C4P_20", "W4C4P_40", "W4C4P_2T0", "W4C4P_4T0", "W4CS4P_20", "W4CS4P_40", "W4CS4P_2T0", "W4CS4P_4T0", "W4C4P4TZ0", "W4C_4P_4TZ0",
               "W6CF6P_2A0", "W6C6P_60", "W6C6P_20", "W6C6P60", "W6CF6PF60", "W6CF6P_2T0", "W6CF6P_2B0", "W6CS6P_20", "W6CS6P_2T0", "W6CS6P_6A0",
               "W6CS6P_6TA0", "W6CS6P_6TB0", "W6CS6P_6B0", "W6C6P_6T0", "W5CF5PF_50", "W6C_6P_6TZ0",
               "W7C7P_20", "W7C17P_20", "W7C17P_7T70", "W7C17P_70", "W7C17P_2T270", "W7C17P_7T27B0", "W7C17P_7T27A0", "W7C17P_7T170", "W7C27P_7T70",
               "W7C27P_7A0", "W7C27P_2T70", "W7C27P_7B0", "W7C27P_2T270", "W7C27P_7T270", "W7CF7P_70", "W7CF7P_2T170",
               "W8C8P_20", "W8C18P_20", "W8C18P_80", "W8C28P_8A0", "W8C28P_8B0", "W8CF8P_80", "W8C18P_2T280", "W8C18P_8T28A0",
               "W8C18P_8T28B0", "W8C18P_8T180", "W8C28P_2T280", "W8CF8P_2T180", "W8C18P_8T80", "W8C18P_8T8Z0", "W8C18P_8T28C0", "W8C18P_8T28Z0",
               "W8C28P_8T80", "W8C28P_8T8Z0", "W8C28P_2T80", "W8C28P_2T8Z0", "W8C28P_8T280", "W8C28P_8T28Z0")

  #TRUE/FALSE on if the variable is a weight at all
  fileFormat$weights <- tolower(fileFormat$variableName) %in% tolower(wgtVars)

  return(fileFormat)
}

#prepares the weight list for the edsurvey.data.frame based on the identified TRUE weights in the fileFormat
buildECLSK2011WeightList <- function(fileFormat){

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

    ujkz <- unique(tolower(grep(paste0("^","(", testJKprefix ,")","[1-9]"), fileFormat$variableName, value = TRUE, ignore.case = TRUE)))
    ujkz <- gsub(tolower(testJKprefix), "", ujkz, fixed = TRUE) #remove jk to leave the numeric values

    #gather the psu variable, it will either be the weight variable name ending in 'psu', OR the variable less the trailing '0' ending in 'psu'
    psuVar <- c(grep(paste0(tempVar, "psu"), varNames, ignore.case = TRUE, value = TRUE),
                grep(paste0(testJKprefix, "psu"), varNames, ignore.case = TRUE, value = TRUE))

    #gather the stratum variable, it will either be the weight variable name ending in 'str', OR the variable less the trailing '0' ending in 'str'
    strVar <- c(grep(paste0(tempVar, "str"), varNames, ignore.case = TRUE, value = TRUE),
                grep(paste0(testJKprefix, "str"), varNames, ignore.case = TRUE, value = TRUE))

    if(length(psuVar)!=1){
      stop(paste0("Cannot find primary sampling unit variable for weight ", sQuote(tempVar), "." ))
    }

    if(length(psuVar)!=1){
      stop(paste0("Cannot find stratum variable for weight ", sQuote(tempVar), "."))
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


#builds the TIMSS dataList object
buildECLS_K2011_dataList <- function(LaF, FF){

  dataList <- list()

  #build the list hierarchical based on the order in which the data levels would be merged in getData
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
