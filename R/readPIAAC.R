#' @title Connect to PIAAC Data
#'
#' @description Opens a connection to a PIAAC data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'              
#' @param path a character value to the full directory path to the PIAAC .csv
#'             files and Microsoft Excel codebook
#' @param countries a character vector of the country/countries to include 
#'                  using the three-digit ISO country code. A list of country
#'                  codes can be found in the PIAAC codebook or
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}.
#'                  If files are downloaded using \code{\link{downloadPIAAC}},
#'                  a country dictionary text file can be
#'                  found in the filepath. You can use \code{*} to indicate
#'                  all countries available.
#' @param forceReread a logical value to force rereading of all processed data.
#'                    Defaults to \code{FALSE}.
#'                    Setting \code{forceReread} to be \code{TRUE} will cause
#'                    PIAAC data to be reread and increase the processing time.
#' @param verbose a logical value that will determine if you want verbose
#'                output while the function is running to indicate the progress.
#'                Defaults to \code{TRUE}.
#'
#' @details
#' Reads in the unzipped .csv files downloaded from the PIAAC dataset using
#' the OECD repository (\url{http://www.oecd.org/skills/piaac/}). Users can use
#' \code{\link{downloadPIAAC}} to download all required files automatically. 
#' 
#' @return
#' an \code{edsurvey.data.frame} for a single specified country or 
#' an \code{edsurvey.data.frame.list} if multiple countries specified
#' @seealso \code{\link{getData}} and \code{\link{downloadPIAAC}}
#' @author Trang Nguyen
#' 
#' @example man/examples/readPIAAC.R
#' @references
#'  Organisation for Economic Co-operation and Development. (2016). \emph{Technical report of the survey of adult skills (PIAAC)} (2nd ed.). Paris, France: Author. Retrieved from \emph{\url{http://www.oecd.org/skills/piaac/PIAAC_Technical_Report_2nd_Edition_Full_Report.pdf}}
#' @importFrom readxl read_excel
#' @export
readPIAAC <- function(path, 
                      countries, 
                      forceReread = FALSE,
                      verbose = TRUE) {
  
  #temporarily adjust any necessary option settings; revert back when done
  userOp <- options(OutDec = ".")
  on.exit(options(userOp), add = TRUE)
  
  filepath <- normalizePath(path, winslash = "/") # to match IEA read-in function
  forceRead <- forceReread # to match IEA read-in function
  
  csvfiles <- list.files(filepath, pattern = "^prg.*\\.csv$", full.names=FALSE, ignore.case=TRUE)
  all_countries <- unique(tolower(substr(csvfiles, 4,6)))
  if (unlist(countries)[1] == "*") {
    countries <- all_countries
  } else {
    countries <- tolower(unlist(countries))
  }
  
  # Process file formats from excel codebook
  ffname <- file.path(filepath,"processed-codebook.meta")
  if(forceRead || !file.exists(ffname)) {
    if (verbose) {
      cat("Processing codebook.\n")
    }
    ff <- processFileFormatReturnFF(filepath)
  } else {
    cacheFile <- tryCatch(readRDS(ffname),
                          error = function(err) {
                            return(NULL)
                          }, warning = function(w) {
                            return(NULL)
                          })
    if (is.null(cacheFile) || cacheMetaReqUpdate(cacheFile$cacheFileVer,"PIAAC")) {
      if (verbose) {
        cat("Processing codebook.\n")
      }
      ff <- processFileFormatReturnFF(filepath)
      forceRead <- TRUE
    } else {
      ff <- cacheFile$ff
    }
  }
  
  # Set up PVs
  # Set up PVS ======================================================================
  # piaacAchievementLevelHelp returns a list of achievement level information for a specific round
  achievement <- piaacAchievementLevelHelp(1) # currently only PIAAC round 1 has available data
  pvs <- list()
  pv_subset <- subset(ff, select = c('Type', 'variableName'),
                      ff$Type != "")
  uniquePvTypes <- tolower(unique(pv_subset$Type))
  for (i in uniquePvTypes) {
    vars <- tolower(pv_subset$variableName[tolower(pv_subset$Type) == i])
    temp_list <- list(varnames = vars)
    checkregex <- sapply(achievement$regex, grepl, i, ignore.case = TRUE)
    subject <- achievement$subjects[which(checkregex)]
    temp_list$achievementLevel <- sort(achievement$achievementLevels[[subject]])
    pvs[[i]] <- temp_list
  }
  attr(pvs, "default") <- names(pvs)[1]
  
  # Process data for each country
  sdf <- list()
  for (cntry in countries) {
    processedData <- processCountryPIAAC(filepath, cntry,ff,forceRead, verbose)
    processedData$userConditions <- list()
    processedData$defaultConditions <- NULL
    processedData$data <- processedData$dataList$student
    
    # Set up weights ===================================================================
    uklz = processedData$cacheFile$reps
    weights <- list(spfwt0 = list(jkbase = "spfwt", jksuffixes = as.character(1:uklz)))
    attr(weights, "default") <- "spfwt0"
    
    processedData$weights <- weights
    processedData$pvvars <- pvs
    processedData$subject <- achievement$subjects
    processedData$year <- "Round 1"
    processedData$assessmentCode <- "International"
    processedData$dataType <- "Adult Data"
    processedData$gradeLevel <- "N/A"
    processedData$achievementLevels <- achievement$achievementLevels
    processedData$omittedLevels <- c("(Missing)", "DON'T KNOW", "NOT STATED OR INFERRED","VALID SKIP","REFUSED",
                                     "DON'T KNOW/REFUSED",'NO RESPONSE','NOT REACHED/NOT ATTEMPTED','ALL ZERO RESPONSE',NA)
    processedData$fileFormat <- ff

    processedData$survey <- "PIAAC"
    processedData$country <- countryDictPIAAC(cntry)
    processedData$jkSumMultiplier <- processedData$cacheFile$jkSumMultiplier
    sdf[[cntry]] <- edsurvey.data.frame(userConditions = processedData$userConditions,
                                        defaultConditions = processedData$defaultConditions,
                                        dataList = buildPIAAC_dataList(processedData$dataList$student,
                                                                       processedData$fileFormat),
                                        weights = processedData$weights,
                                        pvvars = processedData$pvvars,
                                        subject = processedData$subject,
                                        year = processedData$year,
                                        assessmentCode = processedData$assessmentCode,
                                        dataType = processedData$dataType,
                                        gradeLevel = processedData$gradeLevel,
                                        achievementLevels = processedData$achievementLevels,
                                        omittedLevels = processedData$omittedLevels,
                                        survey = processedData$survey,
                                        country = processedData$country,
                                        psuVar = ifelse(as.numeric(processedData$cacheFile$method) == 1,"JK1","varunit"),
                                        stratumVar = ifelse(as.numeric(processedData$cacheFile$method) == 1,"JK1","varstrat"),
                                        jkSumMultiplier = processedData$jkSumMultiplier,
                                        reqDecimalConversion = FALSE)
  }
  
  if(length(sdf) > 1) {
    return(edsurvey.data.frame.list(sdf, labels = countries))
  } else {
    return(sdf[[1]])
  }
}

# @return:
# 1. dataList
# 2. cacheFile: jkSumMultiplier, reps, and jk method
processCountryPIAAC <- function(filepath, countryCode, ff, forceRead, verbose) {
  txtCacheFile <- list.files(filepath, pattern = paste0(countryCode,".*\\.txt$"), full.names = FALSE, ignore.case = TRUE)
  metaCacheFile <- list.files(filepath, pattern = paste0(countryCode,"\\.meta$"), full.names = FALSE, ignore.case = TRUE)
  if (length(txtCacheFile) == 0 || length(metaCacheFile) == 0) {
    forceRead <- TRUE
  } else {
    cacheFile <- tryCatch(readRDS(file.path(filepath,metaCacheFile[1])),
                          error = function(err) {
                            forceRead <<- TRUE
                          },
                          warning = function(w) {
                            forceRead <<- TRUE
                          })
  }
  
  dataList <- list()
  dataListFF <- list()
  
  if(!forceRead) {
    if (verbose) {
      cat(paste0("Found cached data for country code ", dQuote(countryCode),".\n"))
    }
    dataList$student <- getCSVLaFConnection(file.path(filepath,txtCacheFile),ff)
    cacheFile <- readRDS(file.path(filepath,metaCacheFile[1]))
    return(list(dataList = dataList,
                cacheFile = cacheFile))
  }
  if (verbose) {
    cat("Processing data for country code ", dQuote(countryCode),".\n")
  }
  fname = list.files(filepath,pattern = paste0(countryCode,".*\\.csv"), full.names = FALSE, ignore.case = TRUE)
  if(length(fname) == 0) {
    stop("Missing PIAAC data file(s) for country (",countryCode, ") in the path ",sQuote(filepath),".")
  }
  # if using updated data with 2012 and 2017 version of US data, just grab 2012 data.
  if("prgusap1_2012.csv" %in% fname) {
    fname <- "prgusap1_2012.csv"
  }
  if(length(fname) > 1) {
    stop(paste0(countryCode,": there is more than one csv files."))
  }
  fname <- fname[1]
  dat <- read.csv(file.path(filepath,fname), colClasses = "character", na.strings="")
  colnames(dat) <- toupper(colnames(dat))
  
  # checking whether any missing columns in the data file
  # replace with NAs
  missingcolumns <- setdiff(ff$variableName, colnames(dat))
  if(length(missingcolumns) > 0) {
    dat[,missingcolumns] <- NA
  }
  dat <- dat[,ff$variableName]
  
  # replace SAS missing values with SPSS values
  for (ci in 1:nrow(ff)) {
    if (!is.na(ff$replacement[ci]) && ff$replacement[ci] != "") {
      repl <- strsplit(unlist(strsplit(ff$replacement[ci],"^",fixed = TRUE)),"=")
      replv <- sapply(repl, function(x) {
        x[2]
      })
      names(replv) <- sapply(repl, function(x) {
        x[1]
      })
      for (replvi in 1:length(replv)) {
        dat[[ci]] <- gsub(pattern=names(replv)[replvi], replacement=replv[replvi], x=dat[[ci]])
      }
    }
    
    
    # there are some typos in csv files
    if(ff$dataType[ci] != "character") {
      temp <- dat[[ci]]
      suppressWarnings(temp <- as.numeric(temp))
      temp <- format(temp, digits = ff$Width[ci])
      temp <- ifelse(grepl("NA",temp),NA,temp)
      dat[[ci]] <- temp
    }
  }
  # write out processed csv files
  write.csv(dat, file.path(filepath,gsub("\\.csv$",".txt",fname)), col.names = FALSE, na = "")
  
  # return output
  dataList$student <- getCSVLaFConnection(file.path(filepath,gsub("\\.csv",".txt",fname)),
                                          ff)
  dataListFF$student <- ff
  
  # calculate jkSumMultiplier
  # source: https://www.oecd.org/skills/piaac/PIAAC_Technical_Report_2nd_Edition_Full_Report.pdf
  # source: http://www.oecd.org/skills/piaac/PIACTOOLS_16OCT_for_web.pdf
  vemethodn <- unique(dat[,"VEMETHODN"])
  if (length(vemethodn) > 1) {
    warning("There is more than one variance method. Using the first one.")
    vemethodn = vemethodn[1]
  }
  if(vemethodn == 2) {
    jkSumMultiplier <- 1.0
  } else if (vemethodn == 1) {
    jkSumMultiplier <- 79/80 # IDB uses reps = 80 for all countries
  }
  cacheFile <- list(jkSumMultiplier = jkSumMultiplier,
                   method = vemethodn,
                   reps = 80)
  saveRDS(cacheFile,file.path(filepath,paste0("jk",tolower(countryCode),".meta")))
  return(list(dataList = dataList,
              cacheFile = cacheFile))
}

# Reads in excel codebook to return a data.frame fileFormat
processFileFormatReturnFF <- function(filepath) {
  ffname <- list.files(filepath,pattern = "codebook.*\\.xls", ignore.case = TRUE, full.names = FALSE)
  ffname <- file.path(filepath,ffname)
  if (!file.exists(ffname) || length(ffname) == 0) {
    stop(paste0("The codebook Excel file does not exist. It is recommended that users use downloadPIAAC to get all necessary files for the database."))
  }
  codebook <- list()
  codebook$variable <- read_excel(ffname, sheet = 1)
  codebook$value <- read_excel(ffname, sheet = 2)
  
  # retrieve variable information
  ff <- data.frame(variableName = toupper(codebook$variable$Name), 
                   Labels = toupper(codebook$variable$Label),
                   Width = codebook$variable$Width,
                   Decimal = codebook$variable$Decimals,
                   dataType = tolower(codebook$variable$Type), stringsAsFactors = FALSE)
  ff$dataType <- ifelse(grepl("^i",ff$dataType),"integer",
                        ifelse(grepl("^n", ff$dataType), "numeric",
                               ifelse(grepl("^s",ff$dataType),"character",NA)))
  # retrieve value labels
  codebook$value$`Variable Name` <- toupper(codebook$value$`Variable Name`)
  codebook$value$`Value (SAS)` <- gsub("^\\.", "", codebook$value$`Value (SAS)`)
  ff$labelValues <- ""
  ff$replacement <- ""
  ff$missing <- ""
  for (vi in 1:nrow(ff)) {
    v <- ff$variableName[vi]
    dict <- codebook$value[codebook$value$`Variable Name` == v,]
    if(nrow(dict) != 0) {
      ff$labelValues[vi] <- toupper(paste(dict$`Value (SPSS)`,dict$`Value Label`, sep = "=", collapse = "^"))
      repl <- dict[dict$`Value (SAS)` != dict$`Value (SPSS)`,c("Value (SAS)","Value (SPSS)")]
      
      # replacement: CSV data files use SAS missing code
      if (nrow(repl) != 0) {
        ff$replacement[vi] <- paste(repl$`Value (SAS)`, repl$`Value (SPSS)`,sep = "=", collapse = "^")
      }
      
      # missing values
      ff$missing[vi] <- paste(dict$`Value (SPSS)`[dict$`Value Type` == "Missing"], collapse = ";")
      
      # labelled
      ff$labelled[vi] <- any(dict$`Value Type` == "Valid")
      
    } else {
      ff$labelled[vi] <- FALSE
    }
  }
  
  
  # plausible value
  ff$Type <- sapply(ff$variableName, function(zzz){
    if(grepl("^pv.*[0-9]", zzz, ignore.case = TRUE)){
      return(gsub("(pv)|[0-9]","",zzz, ignore.case = TRUE))
    } else  {
      return ("")
    }
  })
  
  # suffix for pvs and weights
  ff$pvWt <- ""
  ff$pvWt <- mapply(function(v,t) {
    if (!grepl("^pv",v, ignore.case = TRUE)) {
      return("")
    } else {
      gsub(paste("pv",t,sep = "|"), "", v, ignore.case=TRUE)
    }
  }, ff$variableName, ff$Type)
  ff$pvWt[grepl("^SPFWT", ff$variableName, ignore.case=TRUE)] <- gsub("[^0-9]", "", ff$variableName[grepl("^SPFWT", ff$variableName, ignore.case=TRUE)])
  ff$pvWt[is.na(ff$pvWt)] <- ""
  ff$weights <- ff$pvWt == 0
  ff$pvWt[ff$pvWt == 0] <- ""
  
  # Process start and end (not used but put here to make it consistent with other read-in functions)
  ff$Start <-  c(1,1 + cumsum(ff$Width))[1:nrow(ff)]
  ff$End <- cumsum(ff$Width)
  
  # column types
  ff$dataType <- ifelse(ff$Width >= 10, gsub("integer","numeric",ff$dataType),ff$dataType)
  # meta file
  cacheFile <- list(ver = packageVersion("EdSurvey"),
                    cacheFileVer=3,
                    ts = Sys.time(),
                    ff = ff)
  saveRDS(cacheFile, file.path(filepath,"processed-codebook.meta"))
  
  # return
  return(ff)
}

countryDictPIAAC <- function(countryCode) {
  dict <- readRDS(system.file("extdata", "PIAACDict.rds", package="EdSurvey"))
  return(dict$Country[dict$CODE == toupper(countryCode)][1])
}

piaacAchievementLevelHelp <- function(round) {
  dict <- readRDS(system.file("extdata", "PIAACAL.rds", package="EdSurvey"))
  dict$level <- paste0("Proficiency ", dict$level)
  ret <- list()
  ret$subjects <- unique(dict$domain)
  ret$regex <- unique(dict$regex)
  ret$default <- ret$subjects[1]
  ret$achievementLevels <- list()
  for (s in ret$subjects) {
    ret$achievementLevels[[s]] <- dict$cutpoints[dict$domain == s]
    names(ret$achievementLevels[[s]]) <- ifelse(is.na(dict$level[dict$domain == s]),"Not Defined", dict$level[dict$domain == s])
  }
  return(ret)
}

#builds the PIRLS dataList object
buildPIAAC_dataList <- function(studentLaf, studentFF){
  
  dataList <- list()
  
  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Student"]] <- dataListItem(lafObject = studentLaf,
                                        fileFormat = studentFF,
                                        levelLabel = "Student",
                                        forceMerge = TRUE,
                                        parentMergeLevels = NULL,
                                        parentMergeVars = NULL,
                                        mergeVars = NULL,
                                        ignoreVars = NULL,
                                        isDimLevel = TRUE)
  
  return(dataList)
}
