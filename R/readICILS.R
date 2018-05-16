#' @title Connect to ICILS Data
#'
#' @description Opens a connection to an ICILS data file residing
#'              on the disk and returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory to the ICILS extracted SPSS (.sav) set of data
#' @param countries a character vector of the country/countries to include using
#'                  the three-digit ISO country code.  
#'                  A list of country codes can be found on Wikipedia at
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes},
#'                  or other online sources. Consult the \emph{ICILS User Guide} to help determine what countries
#'                  are included within a specific testing year of ICILS.
#'                  To select all countries, use a wildcard value of \strong{\code{*}}.
#' @param dataSet a character value of either \code{student} (the default if not specified) or \code{teacher} to 
#'                  indicate which set of data is returned.
#'                  The student-level and teacher-level datasets cannot both be returned at the same time, unlike other IEA datasets.
#'                  
#' @param forceReread a logical value to force rereading of all processed data. 
#'                    The default value of \code{FALSE} will speed up the \code{readICILS} function 
#'                    by using existing read-in data already processed.
#'                    
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#' 
#' @details Reads in the unzipped files downloaded from the ICILS international database(s) using the \href{http://rms.iea-dpc.org/}{IEA Study Data Repository}.
#'          Datafiles require the SPSS datafile (.sav) format using the default filenames.
#'
#' @return
#'  An \code{edsurvey.data.frame} for a single specified country or an \code{edsurvey.data.frame.list} if multiple countries specified.
#'
#' @seealso \code{\link{readNAEP}}, \code{\link{readTIMSS}}, and \code{\link{getData}}
#' @author Tom Fink
#'
#' @example man/examples/readICILS.R
#' 
#' @importFrom haven read_sav
#' @import tibble
#' @export
readICILS <- function(path,
                      countries,
                      dataSet=c("student", "teacher"),
                      forceReread=FALSE,
                      verbose=TRUE) {
  
  path <- normalizePath(unique(path), winslash = "/")
  dataSet <- tolower(dataSet)
  
  if (length(dataSet) > 1){
    dataSet <- dataSet[1]
  }
  if(sum(!(dataSet %in% c("student", "teacher"))>0)){
    stop(paste0("The argument ", sQuote("dataSet"), " must be either ", sQuote("student"), " or ", sQuote("teacher")))
  }
  if(sum(!dir.exists(path)) > 0) { #validate the paths to ensure they all exist
    stop(paste0("Cannot find ", sQuote("path") , "value in ", paste(dQuote(path[!dir.exists(path)]), collapse=", ")))
  }
  if(!is.logical(forceReread)){
    stop(paste0("The argument ", sQuote("forceReread"), " must be a logical value."))
  }
  if(!is.logical(verbose)){
    stop(paste0("The argument ", sQuote("verbose"), " must be a logical value."))
  }
  
  #prepwork
  countries <- tolower(unique(countries))
  gradeLvl <- 8 #ICILS is only 8th grade data
  gradeL <- "b" 
  
  if(unlist(countries)[1]=="*"){ #user has requested data for all countries::grab all country codes
    countries <- unique(tolower(substring(list.files(path, 
                                                     pattern=paste0("^", gradeL, ".....(",
                                                                    paste(getICILSYearCodes(), collapse = "|"), ")\\.sav$"), full.names=FALSE, ignore.case = TRUE),4,6)))
  }
  
  #gather datafile listing::be sure we only pickup ICILS years based on the yearcodes
  filenames <- list.files(path,
                          pattern=paste0("^", gradeL, "..", "(",paste(countries, collapse="|"), ")(",
                                         paste(getICILSYearCodes(), collapse = "|"), ")","\\.sav$"), full.names=TRUE, ignore.case = TRUE)
  if(length(filenames) == 0) {
    stop(paste0("Could not find any ICILS datafiles for countries: ", paste(countries, collapse=", "),
                " in the following folder(s): ", paste(path, collapse=", "), "."))
  }
  
  fSubPart <- tolower(substring(basename(filenames), 1, 8)) #includes a (4th grade), country code, and year code
  fileYrs <- sort(unique(tolower(substring(basename(filenames),7,8))))
  
  procCountryData <- list()
  iProcCountry <- 0 #index counter for adding countries to the list
  
  for(yrCode in fileYrs){ #loop through all the year codes first
    for(cntry in countries){
      
      ICILSfiles <- list()#empty list
      
      #ICILS data can only have student or teacher level data, the unique datasets cannot be joined together
      if(dataSet=="student"){
        ICILSfiles <- c("bcg", #school background
                        "bsg") #student background
      }else{ #teacher level files
        ICILSfiles <- c("bcg", #school background
                        "btg") #teacher background
      }
      
      
      fnames <- NULL # be sure to clear this out so leftovers are not kept from a previous loop
      fnames <- sapply(ICILSfiles, function(f) {
        filenames[(fSubPart %in% paste0(f,cntry, yrCode))] #added check here to only grab files for our specific file code, country, and year
      }, simplify=FALSE)
      
      hasMissing <- sapply(fnames, function(g) {
        sum(nchar(g))==0
      }, simplify=TRUE)
      
      hasExcess <- sapply(fnames, function(h) {
        length(h)>1
      }, simplify=TRUE)
      
      #test if no teacher file exists if we are looking at teacher level data
      if (sum(nchar(unlist(fnames["btg"])))==0 && dataSet=="teacher") {
        warning(paste0("No teacher background file. Skipping Country ", sQuote(cntry), " for year ", sQuote(convertICILSYearCode(yrCode))))
        next
      }
      
      #test for any missing files other than the 'ash' or 'asr' file::also check for any duplicate or multiple files
      if (sum(hasMissing)>0 && sum(nchar(unlist(fnames)))>0) {
        stop(paste0("Missing ICILS Datafile(s) for Country (", cntry, "): ", paste(ICILSfiles[hasMissing], collapse=", "), " for dataset ", sQuote(dataSet)))
      }
      if (sum(hasExcess)>0 && sum(nchar(unlist(fnames)))>0){
        stop(paste0("Excess/Duplicate ICILS Datafile(s) for Country (", cntry, "): ", paste(ICILSfiles[hasExcess], collapse=", "), " for dataset ", sQuote(dataSet)))
      }
      
      #test if there are any files for this country/year combination, if not, we can skip this loop iteration as it does not exist
      if (sum(nchar(unlist(fnames)))==0) {
        warning(paste0("No Data Found. Skipping Country ", sQuote(cntry), " for year ", sQuote(convertICILSYearCode(yrCode))))
        next
      }
      
      iProcCountry <- iProcCountry + 1 #update the processed country index value after we confirm that there is data to process
      processedData <- list()
      
      if(dataSet=="student"){
        processArgs <- list(dataFolderPath = path, 
                            countryCode = cntry, 
                            fnames = fnames, 
                            fileYrs = yrCode, 
                            forceReread = forceReread, 
                            verbose = verbose)
        
        retryProc <- tryCatch({processedData <- do.call("processICILS.Student", processArgs, quote = TRUE)
                                FALSE
                              }, error = function(e){
                                TRUE #flag to retry
                              }, warning = function(w){
                                TRUE #flag to retry
                              })
        
        if (retryProc){
          processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
          processedData <- tryCatch(do.call("processICILS.Student", processArgs, quote = TRUE),
                                    error = function(e){
                                      stop(paste0("Unable to process ICILS Student data for country code ", sQuote(cntry), 
                                                  " having year code ", sQuote(yrCode) ," at folder path(s): ", paste(sQuote(path), collapse = " & "),
                                                  ". Possible file corruption with source data.",
                                                  " Error Message: ", e))
                                    })
        }
        
        processedData$data <- processedData$dataList$student
        processedData$dataSch <- processedData$dataList$school
        processedData$dataTch <- NULL
        
        processedData$fileFormat <- processedData$dataListFF$student
        processedData$fileFormatSchool <- processedData$dataListFF$school
        processedData$fileFormatTeacher <- NULL
        
        #ICILS achievement level definitions includes additional decimal precision for analysis
        processedData$achievementLevels <- c("661.001", "576.001", "492.001", "407.001")
        names(processedData$achievementLevels) <- c("Level 4", "Level 3", "Level 2", "Level 1")
        
        testJKprefix <- c("srwgt") #have any jk prefix values here that are applicable for this dataset
        weights <- NULL #default value
        
        for(i in 1:length(testJKprefix)){
          ujkz <- unique(tolower(grep(paste0("^","(", testJKprefix[i] ,")","[1-9]"), c(names(processedData$dataList$student), names(processedData$dataList$teacher)), value = TRUE, ignore.case = TRUE)))
          ujkz <- gsub(tolower(testJKprefix[i]), "", ujkz, fixed = TRUE) #remove jk to leave the numeric values
          
          if(length(ujkz)>0){
            if(testJKprefix[i]=="srwgt"){
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase="srwgt", jksuffixes=as.character(ujkz))
              names(tmpWgt)[[1]] <- "totwgts"
              weights <- c(weights,tmpWgt)
            }
          }
        }
        
        if(!is.null(weights)){
          attr(weights, "default") <- "totwgts"
        }
        
        processedData$dataType <- "Student"
        processedData$weights <-  weights
        processedData$pvvars <- buildPVVARS_ICILS(processedData$dataListFF$student, defaultPV = "cil")
        processedData$psuVar <- "jkreps"
        processedData$stratumVar <- "jkzones"
        
      }else if(dataSet=="teacher"){
        processArgs <- list(dataFolderPath = path, 
                            countryCode = cntry, 
                            fnames = fnames, 
                            fileYrs = yrCode, 
                            forceReread = forceReread, 
                            verbose = verbose)
        
        retryProc <- tryCatch({processedData <- do.call("processICILS.Teacher", processArgs, quote = TRUE)
                                FALSE
                              }, error = function(e){
                                TRUE #flag to retry
                              }, warning = function(w){
                                TRUE #flag to retry
                              })
        
        if (retryProc){
          processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
          processedData <- tryCatch(do.call("processICILS.Teacher", processArgs, quote = TRUE),
                                    error = function(e){
                                      stop(paste0("Unable to process ICILS Teacher data for country code ", sQuote(cntry), 
                                                  " having year code ", sQuote(yrCode) ," at folder path(s): ", paste(sQuote(path), collapse = " & "),
                                                  ". Possible file corruption with source data.",
                                                  " Error Message: ", e))
                                    })
        }
        
        processedData$data <- processedData$dataList$student
        processedData$dataSch <- processedData$dataList$school
        processedData$dataTch <- NULL
        
        processedData$fileFormat <- processedData$dataListFF$student
        processedData$fileFormatSchool <- processedData$dataListFF$school
        processedData$fileFormatTeacher <- NULL
        
        processedData$achievementLevels <- NULL #no achievement levels for ICILS
        
        processedData$dataType <- "Teacher"
        
        testJKprefix <- c("trwgt") #have any jk prefix values here that are applicable for this dataset
        weights <- NULL #default value
        
        for(i in 1:length(testJKprefix)){
          ujkz <- unique(tolower(grep(paste0("^","(", testJKprefix[i] ,")","[1-9]"), c(names(processedData$dataList$student), names(processedData$dataList$teacher)), value = TRUE, ignore.case = TRUE)))
          ujkz <- gsub(tolower(testJKprefix[i]), "", ujkz, fixed = TRUE) #remove jk to leave the numeric values
          
          if(length(ujkz)>0){
            if(testJKprefix[i]=="trwgt"){
              tmpWgt <- list()
              tmpWgt[[1]] <- list(jkbase="trwgt", jksuffixes=as.character(ujkz))
              names(tmpWgt)[[1]] <- "totwgtt"
              weights <- c(weights,tmpWgt)
              attr(weights, "default") <- "totwgtt"
            }
          }
        }
        
        if(!is.null(weights)){
          attr(weights, "default") <- "totwgtt"
        }
        
        processedData$weights <-  weights
        processedData$pvvars <- list() #no PV values for teacher data
        processedData$psuVar <- "jkrept"
        processedData$stratumVar <- "jkzonet"
        
      }
      
      
      processedData$userConditions <- list()
      processedData$defaultConditions <- NULL
      
      processedData$dataListMeta <- processedData$dataListMeta
      
      processedData$subject <- c("Computer and Information Literacy")
      processedData$year <- convertICILSYearCode(yrCode)
      processedData$assessmentCode <- "International"
      
      processedData$gradeLevel <- "Grade 8"
      
      processedData$omittedLevels <- c('Multiple', NA, 'OMITTED', 'OMITTED OR INVALID', 'LOGICALLY NOT APPLICABLE', 'MISSING', 'NOT ADMINISTERED/MISSING BY DESIGN', 'PRESENTED BUT NOT ANSWERED/INVALID', 'NOT REACHED', 'NOT APPLICABLE', 'NOT STATED', '(Missing)')

      processedData$survey <- "ICILS"
      processedData$country <- getICILSCountryName(cntry)
      
      procCountryData[[iProcCountry]] <- edsurvey.data.frame(userConditions = processedData$userConditions,
                                                      defaultConditions = processedData$defaultConditions,
                                                      data = processedData$dataList$student,
                                                      dataSch = processedData$dataList$school,
                                                      dataTch = processedData$dataList$teacher,
                                                      dataListMeta <- processedData$dataListMeta,
                                                      weights = processedData$weights,
                                                      pvvars = processedData$pvvars,
                                                      subject = processedData$subject,
                                                      year = processedData$year,
                                                      assessmentCode = processedData$assessmentCode,
                                                      dataType = processedData$dataType,
                                                      gradeLevel = processedData$gradeLevel,
                                                      achievementLevels = processedData$achievementLevels,
                                                      omittedLevels = processedData$omittedLevels,
                                                      fileFormat = processedData$fileFormat,
                                                      fileFormatSchool = processedData$fileFormatSchool,
                                                      fileFormatTeacher = processedData$fileFormatTeacher,
                                                      survey = processedData$survey,
                                                      country = processedData$country,
                                                      psuVar = processedData$psuVar,
                                                      stratumVar = processedData$stratumVar,
                                                      jkSumMultiplier = 1.0) #defined by the method of JK weight replication used (JK1)
    }#end country loop
  }#end for(fileYr in fileYrs)
  
  
  if (iProcCountry > 1) {
    return(edsurvey.data.frame.list(procCountryData)) #do not apply country labels::the edsurvey.data.frame.list does a better job of detecting the differences
  } else {
    # just one country
    return(procCountryData[[1]])
  }
}

#@param yrCode a character value used in the ICILS filenaming structure to identify the specific year (e.g. m1, m2, m6)
#@return a numeric 4 digit year value
convertICILSYearCode <- function(yrCode){
  
  yrTest <- tolower(sort(unique(yrCode)))
  yrTest[yrTest %in% "i1"] <- 2013
  
  return(yrTest)
}

getICILSYearCodes <- function(){
  #retrieve the TIMMS years based on their filenaming structure
  
  yrVals = c("i1")
  names(yrVals) = c(2013)
  
  return(yrVals)
}

#builds the list of pvvars from the passed fileformat data.frame
buildPVVARS_ICILS <- function(fileFormat, defaultPV = "cil"){
  
  pvFields <- subset(fileFormat, nchar(fileFormat$Type)>0) #type is identified in writeTibbleToFWFReturnFileFormat function
  constructs <- unique(pvFields$Type)
  pvvars <- vector("list", length(constructs))
  names(pvvars) <- constructs
  
  for(i in names(pvvars)){
    varList <- tolower(sort(pvFields$variableName[pvFields$Type == i]))
    pvvars[[i]] <- list(varnames=varList)
  }
  
  #test if defaultPV in the list and make it default::otherwise set it to the first pvvar in the list
  if (defaultPV %in% names(pvvars)){
    attr(pvvars, "default") <- defaultPV
  }else{
    attr(pvvars, "default") <- names(pvvars)[1]
  }
  
  return (pvvars)
}

#process the ICILS student level data..be sure the cache files are seperate from the teacher cache files
processICILS.Student <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
  
  yearCode <- unlist(fileYrs)[1]
  metaCacheFP <- list.files(dataFolderPath,
                            pattern=paste0("^bs", "(",paste(countryCode), ")",
                                           yearCode, "\\.meta$"), full.names=TRUE, ignore.case = TRUE)
  
  #grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
                            pattern=paste0("^b[sc].", "(",paste(countryCode), ")",
                                           yearCode, "\\.txt$"), full.names=TRUE, ignore.case = TRUE)
  
  #determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if(length(metaCacheFP)==0 || length(txtCacheFWF)<2 || forceReread==TRUE){ #ensure we have a full dataset of cache files
    runProcessing <- TRUE
  }else{
    cacheFile <- readRDS(unlist(metaCacheFP)[1])
    
    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "ICILS")){ #cacheMetaReqUpdates in its own R file
      runProcessing <- TRUE
    }else{
      #rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="bsg"], cacheFile$dataListFF$student)
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="bcg"], cacheFile$dataListFF$school)

      
      dataList <- list(student = studentLAF, school = schoolLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- cacheFile$dataListFF
      dataListMeta <- cacheFile$dataListMeta
      
      runProcessing <- FALSE
    }
  } #end if(length(metaCacheFP)==0 || length(txtCacheFWF)<2 || forceReread==TRUE)
  
  if(runProcessing==TRUE){
    
    if(verbose==TRUE){
      cat(paste0("Processing Data for Country: ", dQuote(countryCode),"\n"))
    }
    
    #SCHOOL LEVEL===================================================
    bcg <- unlist(fnames["bcg"])[1]
    schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames["bcg"])[1], ignore.case = TRUE)
    schoolDF1 <- read_sav(bcg)  
    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP )  
    #===============================================================
    
    #STUDENT LEVEL==================================================
    bsg <- unlist(fnames["bsg"])[1]
    stuDF1 <- read_sav(bsg)
    colnames(stuDF1) <- toupper(colnames(stuDF1))
    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames["bsg"])[1], ignore.case = TRUE)
    ffstu <- writeTibbleToFWFReturnFileFormat(stuDF1, stuFP)  
    #===============================================================

    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    
    #build data list and link metadata object=======================
    dataList <- list(student = studentLAF, school = schoolLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
    dataListFF <- list(student = ffstu, school = ffsch)
    
    dataListMeta <- list(student = "", school = "")
    dataListMeta$student <- list(school = "idcntry;idschool") #idschool #do we need to merge on idclass var as well?
    dataListMeta$school <- list()
    #===============================================================
    
    #save the cachefile to be read-in for the next call
    cacheFile <- list(ver=packageVersion("EdSurvey"),
                      cacheFileVer=2,
                      ts=Sys.time(),
                      dataListFF=dataListFF,
                      dataListMeta=dataListMeta)
    
    saveRDS(cacheFile, file.path(dataFolderPath,paste0("bs", countryCode, yearCode,".meta")))
    
  } else { #used the cache files
    if(verbose==TRUE){
      cat(paste0("Found cached data for country code ", dQuote(countryCode),"\n"))
    }
  } #end if(runProcessing==TRUE)
  
  return(list(dataList = dataList,
              dataListFF = dataListFF,
              dataListMeta = dataListMeta)) 
}

#process the ICILS student level data..be sure the cache files are seperate from the teacher cache files
processICILS.Teacher <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
  
  yearCode <- unlist(fileYrs)[1]
  metaCacheFP <- list.files(dataFolderPath,
                            pattern=paste0("^bt", "(",paste(countryCode), ")",
                                           yearCode, "\\.meta$"), full.names=TRUE, ignore.case = TRUE)
  
  #grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
                            pattern=paste0("^b[tc].", "(",paste(countryCode), ")",
                                           yearCode, "\\.txt$"), full.names=TRUE, ignore.case = TRUE)
  
  #determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if(length(metaCacheFP)==0 || length(txtCacheFWF)<2 || forceReread==TRUE){ #ensure we have a full dataset of cache files
    runProcessing <- TRUE
  }else{
    cacheFile <- readRDS(unlist(metaCacheFP)[1])
    
    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "ICILS")){ #cacheMetaReqUpdates resides in its own R file
      runProcessing <- TRUE
    }else{
      #rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="btg"], cacheFile$dataListFF$student) #we will treat the teachers as students as the root level of data
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="bcg"], cacheFile$dataListFF$school)
      
      
      dataList <- list(student = studentLAF, school = schoolLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- cacheFile$dataListFF
      dataListMeta <- cacheFile$dataListMeta
      
      runProcessing <- FALSE
    }
  } #end if(length(metaCacheFP)==0 || length(txtCacheFWF)<2 || forceReread==TRUE)
  
  if(runProcessing==TRUE){
    
    if(verbose==TRUE){
      cat(paste0("Processing Data for Country: ", dQuote(countryCode),"\n"))
    }
    
    #SCHOOL LEVEL===================================================
    bcg <- unlist(fnames["bcg"])[1]
    schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames["bcg"])[1], ignore.case = TRUE)
    schoolDF1 <- read_sav(bcg)  
    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP )  
    #===============================================================
    
    #teachers will be using the 'student' level vars in this situation as the root data level
    #TEACHER LEVEL==================================================
    btg <- unlist(fnames["btg"])[1]
    hasTeacherData <- FALSE
    
    if(min(is.na(btg)) == 0) { #test this in the main readICILS to ensure we have a teacher file before processing
      tchDF1 <- read_sav(btg)
      colnames(tchDF1) <- toupper(colnames(tchDF1))
      tchFP <- gsub(".sav$", "\\.txt", unlist(fnames["btg"])[1], ignore.case = TRUE)
      ffTch <- writeTibbleToFWFReturnFileFormat(tchDF1, tchFP) 
    }
    #===============================================================
    
    
    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(tchFP, ffTch)
    
    #build data list and link metadata object=======================
    dataList <- list(student = studentLAF, school = schoolLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
    dataListFF <- list(student = ffTch, school = ffsch)
    
    dataListMeta <- list(student = "", school = "")
    dataListMeta$student <- list(school = "idcntry;idschool") #setup link vars
    dataListMeta$school <- list()
    #===============================================================

    #save the cachefile to be read-in for the next call
    cacheFile <- list(ver=packageVersion("EdSurvey"),
                      cacheFileVer=2,
                      ts=Sys.time(),
                      dataListFF=dataListFF,
                      dataListMeta=dataListMeta)
    
    saveRDS(cacheFile, file.path(dataFolderPath,paste0("bt", countryCode, yearCode,".meta")))
    
  } else { #used the cache files
    if(verbose==TRUE){
      cat(paste0("Found cached data for country code ", dQuote(countryCode),"\n"))
    }
  } #end if(runProcessing==TRUE)
  
  return(list(dataList = dataList,
              dataListFF = dataListFF,
              dataListMeta = dataListMeta)) 
}

exportICILSToCSV <- function(folderPath, exportPath, cntryCodes, dataSet, ...){
  
  sdfList <- readICILS(folderPath, cntryCodes, dataSet, ...)
  
  if (class(sdfList) == "edsurvey.data.frame.list"){
    for(i in 1:length(sdfList$datalist)){
      
      sdf  <- sdfList$datalist[[i]]
      cntry <- sdf$country
      
      cat(paste(cntry, "Working...."))
      data <- getData(sdf, names(sdf), dropUnusedLevels = FALSE, omittedLevels = FALSE)
      
      
      write.csv(data, file=file.path(exportPath, paste0(cntry, ".csv")), na="", row.names = FALSE)
      cat(paste(cntry, "Completed"), "\n")
    }
  } else if (class(sdfList) == "edsurvey.data.frame"){
    
    sdf <- sdfList
    cntry <- sdf$country
    
    cat(paste(cntry, "Working...."))
    data <- getData(sdf, names(sdf), dropUnusedLevels = FALSE, omittedLevels = FALSE)
    
    write.csv(data, file=file.path(exportPath, paste0(cntry, ".csv")), na="", row.names = FALSE)
    cat(paste(cntry, "Completed"), "\n")
  }
  
}


#get the full country name to aide the user, so they won't have to track them down.
#cntryCode should be the 3 character country code vector defined in the data filename scheme (e.g., usa = United States, swe = Sweden)
#if a match is not found, this funtion will return a character value indicating it is unknown '(unknown) CountryCode: xxx'
getICILSCountryName <- function(countryCode){
  
  cntryCodeDF <- data.frame(
    cntryCode = c("aba", "aus",
                  "che", "chl", "cnl", "cot", "cze",
                  "deu", "dnk",
                  "hkg", "hrv",
                  "kor",
                  "ltu",
                  "nld", "nor",
                  "pol",
                  "rus",
                  "svk", "svn",
                  "tha", "tur"),
    cntryName = c("Buenos Aires, Argentina", "Australia",
                  "Switzerland", "Chile", "Newfoundland and Labrador, Canada", "Ontario, Canada", "Czech Republic",
                  "Germany", "Denmark",
                  "Hong Kong SAR", "Croatia",
                  "Korea, Rep. of",
                  "Lithuania",
                  "Netherlands", "Norway",
                  "Poland",
                  "Russian Federation",
                  "Slovak Republic", "Slovenia",
                  "Thailand", "Turkey"),
    stringsAsFactors = FALSE) #be sure to not create any factors::factors not needed at all
  
  lookupNames <- vector(mode = "character", length = length(countryCode))
  
  for(i in 1:length(countryCode)){
    testName <- cntryCodeDF[cntryCodeDF$cntryCode==countryCode[i], "cntryName"]
    
    if(length(testName)==0){ #test if no value found
      testName <- paste("(unknown) CountryCode:", countryCode[i])
    }
    
    lookupNames[i] <- testName
  }
  
  return(lookupNames)
}