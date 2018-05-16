#' @title Connect to PIRLS Data
#'
#' @description Opens a connection to a PIRLS data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory to the PIRLS extracted SPSS (.sav) set of data.
#' @param countries a character vector of the country/countries to include using
#'                  the three-digit ISO country code.  
#'                  A list of country codes can be found on Wikipedia at
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes},
#'                  or other online sources. Consult the \emph{PIRLS User Guide} to help determine what countries
#'                  are included within a specific testing year of PIRLS.
#'                  To select all countries, use a wildcard value of \strong{\code{*}}.
#' @param forceReread a logical value to force rereading of all processed data. 
#'                    The default value of \code{FALSE} will speed up the \code{readPIRLS} function by 
#'                    using existing read-in data already processed.
#'                    
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#' 
#' @details Reads in the unzipped files downloaded from the PIRLS international database(s) using the \href{http://rms.iea-dpc.org/}{IEA Study Data Repository}. 
#'          Datafiles require the SPSS datafile (.sav) format using the default filenames.
#'
#' @details A PIRLS \code{edsurvey.data.frame} includes three distinct data levels: 
#'          \itemize{
#'               \item student
#'               \item school
#'               \item teacher
#'          }
#'           
#'          When the \code{getData} function is called using a PIRLS \code{edsurvey.data.frame},
#'          the requested data variables are inspected, and it handles any necessary data merges automatically. 
#'          Note that the \code{school} data will always be returned merged to the \code{student}
#'          data, even if only \code{school} variables are requested.
#'          Only if \code{teacher} variables are requested by the \code{getData} call, will cause \code{teacher} data to be merged.
#'          Many \code{students} can be linked to many \code{teachers}, which varies widely between countries.
#'          
#' @details Please note that calling the \code{dim} function for a PIRLS \code{edsurvey.data.frame} will result in 
#'          the row count as if the \code{teacher} dataset was merged.
#'          This row count will be considered the \code{full data N} of the \code{edsurvey.data.frame}, even if no \code{teacher} data were included in an analysis.  
#'          The column count returned by \code{dim} will be the count of unique column variables across all three data levels.
#'          
#' @return
#'  an \code{edsurvey.data.frame} for a single specified country or an \code{edsurvey.data.frame.list} if multiple countries specified
#'
#' @seealso \code{\link{readNAEP}}, \code{\link{readTIMSS}}, \code{\link{getData}}, and \code{\link{downloadPIRLS}}
#' @author Tom Fink
#'
#' @example man/examples/readPIRLS.R
#' 
#' @importFrom haven read_sav
#' @import tibble
#' @export
readPIRLS <- function(path,
                      countries,
                      forceReread=FALSE,
                      verbose=TRUE) {
  
  path <- normalizePath(unique(path), winslash = "/")
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
  gradeLvl <- 4 #PIRLS is only 4th grade data
  gradeL <- "a" 
  
  if(unlist(countries)[1]=="*"){ #user has requested data for all countries::grab all country codes
    countries <- unique(tolower(substring(list.files(path, 
                                                     pattern=paste0("^", gradeL, ".....(",
                                                     paste(getPIRLSYearCodes(), collapse = "|"), ")\\.sav$"), full.names=FALSE, ignore.case = TRUE),4,6)))
  }
  
  #gather datafile listing::be sure we only pickup PIRLS years based on the yearcodes
  filenames <- list.files(path,
                          pattern=paste0("^", gradeL, "..", "(",paste(countries, collapse="|"), ")(",
                                         paste(getPIRLSYearCodes(), collapse = "|"), ")","\\.sav$"), full.names=TRUE, ignore.case = TRUE)
  if(length(filenames) == 0) {
    stop(paste0("Could not find any PIRLS datafiles for countries: ", paste(countries, collapse=", "),
                " in the following folder(s): ", paste(path, collapse=", "), "."))
  }
  
  fSubPart <- tolower(substring(basename(filenames), 1, 8)) #includes a (4th grade), country code, and year code
  fileYrs <- sort(unique(tolower(substring(basename(filenames),7,8))))
  
  procCountryData <- list()
  iProcCountry <- 0 #index counter for adding countries to the list
  
  for(yrCode in fileYrs){ #loop through all the year codes first
    for(cntry in countries){
      
      PIRLSfiles <- list()#empty list
      
      PIRLSfiles <- c("acg", #school background
                      "asa", #student achievement
                      "asg", #student background
                      "ash", #student home background (Special file::might not always be present)
                      "asr", #within-country scoring reliability
                      "ast", #student-teacher linkage
                      "atg") #teacher background
  
      
      fnames <- NULL # be sure to clear this out so leftovers are not kept from a previous loop
      fnames <- sapply(PIRLSfiles, function(f) {
        filenames[(fSubPart %in% paste0(f,cntry, yrCode))] #added check here to only grab files for our specific grade level, country, and year
      }, simplify=FALSE)
      
      hasMissing <- sapply(fnames, function(g) {
        sum(nchar(g))==0
      }, simplify=TRUE)
  
      hasExcess <- sapply(fnames, function(h) {
        length(h)>1
      }, simplify=TRUE)
      
      #test for any missing files other than the 'ash' or 'asr' file::also check for any duplicate or multiple files
      if (sum(hasMissing)>0 && sum(nchar(unlist(fnames)))>0) {
        stop(paste0("Missing PIRLS Datafile(s) for Country (", cntry, "): ", paste(PIRLSfiles[hasMissing], collapse=", ")))
      }
      if (sum(hasExcess)>0 && sum(nchar(unlist(fnames)))>0){
        stop(paste0("Excess/Duplicate PIRLS Datafile(s) for Country (", cntry, "): ", paste(PIRLSfiles[hasExcess], collapse=", ")))
      }
      
      #test if there are any files for this country/year combination, if not, we can skip this loop iteration as it does not exist
      if (sum(nchar(unlist(fnames)))==0) {
        warning(paste0("No Data Found. Skipping Country ", sQuote(cntry), " for year ", sQuote(convertPIRLSYearCode(yrCode))))
        next
      }
  
      iProcCountry <- iProcCountry + 1 #update the processed country index value after we confirm that there is data to process
      processedData <- list()
      
      processArgs <- list(dataFolderPath = path, 
                          countryCode = cntry, 
                          fnames = fnames, 
                          fileYrs = yrCode, 
                          forceReread = forceReread, 
                          verbose = verbose)
      
      retryProc <- tryCatch({processedData <- do.call("processPIRLS", processArgs, quote = TRUE)
                              FALSE
                            }, error = function(e){
                              TRUE #flag to retry
                            }, warning = function(w){
                              TRUE #flag to retry
                            })
      
      if (retryProc){
        processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
        processedData <- tryCatch(do.call("processPIRLS", processArgs, quote = TRUE),
                                  error = function(e){
                                    stop(paste0("Unable to process PIRLS data for country code ", sQuote(cntry), 
                                                " having year code ", sQuote(yrCode) ," at folder path(s): ", paste(sQuote(path), collapse = " & "),
                                                ". Possible file corruption with source data.",
                                                " Error Message: ", e))
                                  })
      }
        
      processedData$userConditions <- list()
      processedData$defaultConditions <- NULL
      processedData$data <- processedData$dataList$student
      processedData$dataSch <- processedData$dataList$school
      processedData$dataTch <- processedData$dataList$teacher
      
      processedData$dataListMeta <- processedData$dataListMeta
      
      testJKprefix <- c("JK", "JK.TCHWGT") #have any jk prefix values here that are applicable for this dataset
      weights <- NULL #default value
      
      for(i in 1:length(testJKprefix)){
        ujkz <- unique(tolower(grep(paste0("^","(", testJKprefix[i] ,")","[1-9]"), c(names(processedData$dataList$student), names(processedData$dataList$teacher)), value = TRUE, ignore.case = TRUE)))
        ujkz <- gsub(tolower(testJKprefix[i]), "", ujkz, fixed = TRUE) #remove jk to leave the numeric values
        
        if(length(ujkz)>0){
          if(testJKprefix[i]=="JK"){
            tmpWgt <- list()
            tmpWgt[[1]] <- list(jkbase="jk", jksuffixes=as.character(ujkz))
            names(tmpWgt)[[1]] <- "totwgt"
            weights <- c(weights,tmpWgt)
          }
          if(testJKprefix[i]=="JK.TCHWGT"){
            tmpWgt <- list()
            tmpWgt[[1]] <- list(jkbase="jk.tchwgt", jksuffixes=as.character(ujkz))
            names(tmpWgt)[[1]] <- "tchwgt"
            weights <- c(weights,tmpWgt)
          }
        }
      }
      attr(weights, "default") <- "totwgt"
      
      processedData$weights <-  weights
      processedData$pvvars <- buildPVVARS_PIRLS(processedData$dataListFF$student, defaultPV = "rrea")
      processedData$subject <- c("Reading")
      processedData$year <- convertPIRLSYearCode(yrCode)
      processedData$assessmentCode <- "International"
      processedData$dataType <- "Student Data"
      processedData$gradeLevel <- "Grade 4"
      
      #achievment level cutpoints as defined by PIRLS documentation
      processedData$achievementLevels <- c("625", "550", "475", "400")
      names(processedData$achievementLevels) <- c("Advanced International Benchmark", "High International Benchmark", "Intermediate International Benchmark", "Low International Benchmark")
      
      processedData$omittedLevels <- c('Multiple', NA, 'N/A', 'NA', 'OMITTED', 'OMITTED OR INVALID', 'NOT REACHED', 'INVALID RESPONSE', 'NOT APPLICABLE', 'LOGICALLY NOT APPLICABLE', 'MISSING', '(Missing)')

      processedData$fileFormat <- processedData$dataListFF$student
      processedData$fileFormatSchool <- processedData$dataListFF$school
      processedData$fileFormatTeacher <- processedData$dataListFF$teacher
      processedData$survey <- "PIRLS"
      processedData$country <- getPIRLSCountryName(cntry)
      
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
                                                             psuVar = "jkrep",
                                                             stratumVar = "jkzone",
                                                             jkSumMultiplier = 0.5) #defined by the method of JK weight replication used (JK2)
    }#end country loop
  }#end for(fileYr in fileYrs)
  
  
  if (iProcCountry > 1) {
      return(edsurvey.data.frame.list(procCountryData)) #return full list.  let edsurvey.data.frame.list constructor build covs
  } else {
    # just one country
    return(procCountryData[[1]])
  }
}

#@param yrCode a character value used in the PIRLS filenaming structure to identify the specific year (e.g. m1, m2, m6)
#@return a numeric 4 digit year value
convertPIRLSYearCode <- function(yrCode){
  
  yrTest <- tolower(sort(unique(yrCode)))
  yrTest[yrTest %in% "r1"] <- 2001
  yrTest[yrTest %in% "r2"] <- 2006
  yrTest[yrTest %in% "r3"] <- 2011

  return(yrTest)
}

getPIRLSYearCodes <- function(){
  #retrieve the TIMMS years based on their filenaming structure
  
  yrVals = c("r1","r2","r3")
  names(yrVals) = c(2001, 2006, 2011)

  return(yrVals)
}

#builds the list of pvvars from the passed fileformat data.frame
buildPVVARS_PIRLS <- function(fileFormat, defaultPV = "rrea"){
  
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


#@param dataFolderPath a character value of the initial folder path provided to the 'readPIRLS' call to find the .sav SPSS files
#@param countryCode a character value of the 3-digit country code we want to process
#@param fnames a character vector of the specific filenames that are needed for this country, generally there should be 7 files specified
processPIRLS <- function(dataFolderPath, countryCode, fnames, fileYrs, forceReread, verbose) {
  
  yearCode <- unlist(fileYrs)[1]
  metaCacheFP <- list.files(dataFolderPath,
                            pattern=paste0("^a", "(",paste(countryCode), ")",
                                           yearCode, "\\.meta$"), full.names=TRUE, ignore.case = TRUE)
  
  #grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
                            pattern=paste0("^a..", "(",paste(countryCode), ")",
                                           yearCode, "\\.txt$"), full.names=TRUE, ignore.case = TRUE)
  
  #determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE){ #ensure we have a full dataset of cache files
    runProcessing <- TRUE
  }else{
    cacheFile <- readRDS(unlist(metaCacheFP)[1])
    
    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "PIRLS")){ #cacheMetaReqUpdates resides in its own R file
      runProcessing <- TRUE
    }else{
      #rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="asg"], cacheFile$dataListFF$student)
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="acg"], cacheFile$dataListFF$school)
      teacherLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))=="atg"], cacheFile$dataListFF$teacher)
      
      dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- cacheFile$dataListFF
      dataListMeta <- cacheFile$dataListMeta
      
      runProcessing <- FALSE
    }
  } #end if(length(metaCacheFP)==0 || length(txtCacheFWF)<3 || forceReread==TRUE)
  
  if(runProcessing==TRUE){
    
    if(verbose==TRUE){
      cat(paste0("Processing Data for Country: ", dQuote(countryCode),"\n"))
    }
  
    #SCHOOL LEVEL===================================================
    acg <- unlist(fnames["acg"])[1]
    schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames["acg"])[1], ignore.case = TRUE)
    schoolDF1 <- read_sav(acg)  
    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP )  
    #===============================================================
    
    #STUDENT LEVEL==================================================
    asa <- unlist(fnames["asa"])[1]
    asg <- unlist(fnames["asg"])[1]
    ash <- unlist(fnames["ash"])[1]
    asr <- unlist(fnames["asr"])[1]
    stuDF1 <- read_sav(asa)
    colnames(stuDF1) <- toupper(colnames(stuDF1))
    ids1 <- grep("^ID", names(stuDF1), ignore.case=TRUE, value=TRUE)
    stuDF2 <- read_sav(asg)
    colnames(stuDF2) <- toupper(colnames(stuDF2))
    ids2 <- grep("^ID", names(stuDF2), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDPUNCH", "IDGRADER"))] #IDPUNCH should be omitted for merging
    
    mm <- mergeTibble(stuDF1,
                      stuDF2,
                      by=ids12,
                      all.x=FALSE,
                      all.y=FALSE, 
                      suffixes=c("", ".junk"))
    mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
    
    if(nrow(stuDF1) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("asa"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }
    if(nrow(stuDF2) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("asg"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }
    if(min(is.na(ash)) == 0) {
      stuDF3 <- read_sav(ash)
      colnames(stuDF3) <- toupper(colnames(stuDF3))
      ids3 <- grep("^ID", names(stuDF3), ignore.case=TRUE, value=TRUE)
      idsmm3 <- ids12[ids12 %in% ids3]
      idsmm3 <- idsmm3[!(idsmm3 %in% c("IDPUNCH", "IDGRADER"))] #IDPUNCH should be omitted for merging
      
      nr <- nrow(mm)
      mm <- mergeTibble(mm,
                        stuDF3,
                        by=idsmm3,
                        all.x=TRUE,
                        all.y=FALSE, 
                        suffixes=c("", ".junk"))
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
      if(nrow(stuDF1) != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance"))
      }
      if(nr != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("ash"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance"))
      }
    } else {
      idsmm3 <- ids12
    }
    if(min(is.na(asr)) == 0){
      stuDF4 <- read_sav(asr)
      colnames(stuDF4) <- toupper(colnames(stuDF4))
      ids4 <- grep("^ID", names(stuDF4), ignore.case=TRUE, value=TRUE)
      idsmm4 <- idsmm3[idsmm3 %in% ids4]
      idsmm4 <- idsmm4[!(idsmm4 %in% c("IDPUNCH", "IDGRADER"))] #IDPUNCH should be omitted for merging
      
      #test here for duplicate rows::special case for PIRLS 2001 for 'HKG' datafile having multiple data rows
      #anyDuplicated will return the row index of the first duplicate found. if no duplicates found, then it returns '0'
      if(anyDuplicated(stuDF4)>0){
        stuDF4 <- dropTibbleDupes(stuDF4)
      }
      
      nr <- nrow(mm)
      mm <- mergeTibble(mm,
                        stuDF4,
                        by=idsmm4,
                        all.x=TRUE,
                        all.y=FALSE, 
                        suffixes=c("", ".junk"))
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
      if(nr != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote("asr"), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance"))
      }
      mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
    }
    
    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames["asg"])[1], ignore.case = TRUE)
    ffstu <- writeTibbleToFWFReturnFileFormat(mm, stuFP)  
    #===============================================================
    
    #Student-Teacher Linkage and Teacher Background=================
    ast <- unlist(fnames["ast"])[1]
    atg <- unlist(fnames["atg"])[1]
    
    stuTeachDF <- read_sav(ast)
    colnames(stuTeachDF) <- toupper(colnames(stuTeachDF))
    teachDF <- read_sav(atg)
    colnames(teachDF) <- toupper(colnames(teachDF))
    
    ids1 <- grep("^ID", names(stuTeachDF), ignore.case=TRUE, value=TRUE)
    ids2 <- grep("^ID", names(teachDF), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDPUNCH", "IDGRADER", "IDCLASS"))] #IDPUNCH should be omitted for merging
    
    mm <- mergeTibble(stuTeachDF,
                      teachDF,
                      by=ids12,
                      all.x=TRUE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))
    mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
    
    if(nrow(stuTeachDF) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote("atg"), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }
    
    teachFP <- gsub(".sav$", "\\.txt", unlist(fnames["atg"])[1], ignore.case = TRUE)
    ffTeach <- writeTibbleToFWFReturnFileFormat(mm, teachFP)
    #===============================================================
    
    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    teacherLAF <- getFWFLaFConnection(teachFP, ffTeach)
    
    #build data list and link metadata object=======================
    dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
    dataListFF <- list(student = ffstu, school = ffsch, teacher = ffTeach)
    
    dataListMeta <- list(student = "", school = "", teacher = "")
    dataListMeta$student <- list(school = "idcntry;idschool", teacher = "idcntry;idstud") #idschool #do we need to merge on idclass var as well?
    dataListMeta$school <- list()
    dataListMeta$teacher <- list()
    #===============================================================
    
    #save the cachefile to be read-in for the next call
    cacheFile <- list(ver=packageVersion("EdSurvey"),
                      cacheFileVer=3,
                      ts=Sys.time(),
                      dataListFF=dataListFF,
                      dataListMeta=dataListMeta)
    
    saveRDS(cacheFile, file.path(dataFolderPath,paste0("a", countryCode, yearCode,".meta")))
    
  } else { #used the cache files
    if(verbose==TRUE){
      cat(paste0("Found cached data for country code ", dQuote(countryCode),"\n"))
    }
  } #end if(runProcessing==TRUE)
  
  return(list(dataList = dataList,
              dataListFF = dataListFF,
              dataListMeta = dataListMeta)) 
}


exportPIRLSToCSV <- function(folderPath, exportPath, cntryCodes, ...){
  
    sdfList <- readPIRLS(folderPath, cntryCodes, ...)
    
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

#when a tibble has rows dropped/removed from it, the field attributes are striped from the columns, this preserves the column attributes
# \code{a} will be the tibble in which we want to remove the duplicates
#borrowed code from the 'mergeTibble' function in 'readTIMSS.r' file
dropTibbleDupes <- function(a) {
  
  ab <- a[!duplicated(a), ]
  cols <- names(ab)
  abt <- as_tibble(ab)
  
  for(i in 1:length(cols)) {
    coli <- cols[i]
    abcoli <- abt[[coli]]
    if(coli %in% names(a)) {
      ocoli <- a[[coli]]

      newAtrs <- attributes(ocoli)
      oldAnames <- names(attributes(abcoli))
      transname <- names(newAtrs)
      transname <- transname[!transname %in% oldAnames]
      for(tri in 1:length(transname)) {
        if((!is.null(transname[tri])) && (!is.na(transname[tri])) && (length(transname[tri])>0)){
          attr(abcoli, transname[tri]) <- newAtrs[[transname[tri]]]
        }
      }
      abt[[coli]] <- abcoli
    }
  }
  return(abt)
}

#get the full country name to aide the user, so they won't have to track them down.
#cntryCode should be the 3 character country code vector defined in the data filename scheme (e.g., usa = United States, swe = Sweden)
#if a match is not found, this funtion will return a character value indicating it is unknown '(unknown) CountryCode: xxx'
getPIRLSCountryName <- function(countryCode){
  
  cntryCodeDF <- data.frame(
    cntryCode = c("aad", "adu", "are", "arg", "aus", "aut", "aze",
                  "bfl", "bfr", "bgr", "blz", "bwa",
                  "cab", "can", "cbc", "cns", "col", "cot", "cqu", "cyp", "cze",
                  "deu", "dnk",
                  "ean", "eng", "esp",
                  "fin", "fra",
                  "geo", "grc",
                  "hkg", "hnd", "hrv", "hun",
                  "idn", "irl", "irn", "is5", "isl", "isr", "ita",
                  "kwt",
                  "ltu", "lux", "lva",
                  "ma6", "mar", "mda", "mkd", "mln", "mlt",
                  "nir", "nld", "no5", "nor", "nzl",
                  "omn",
                  "pol", "prt",
                  "qat",
                  "rom", "rus",
                  "sau", "sco", "se3", "sgp", "svk", "svn", "swe",
                  "tto", "tur", "twn",
                  "usa",
                  "zaf"),
    cntryName = c("Abu Dhabi, UAE", "Dubai, UAE", "United Arab Emirates", "Argentina", "Australia", "Austria", "Azerbaijan",
                  "Belgium (Flemish)", "Belgium (French)", "Bulgaria", "Belize", "Botswana",
                  "Alberta, Canada", "Canada", "British Columbia, Canada", "Nova Scotia, Canada", "Colombia", "Ontario, Canada", "Quebec, Canada", "Cyprus", "Czech Republic",
                  "Germany", "Denmark",
                  "Andalusia, Spain", "England", "Spain",
                  "Finland", "France",
                  "Georgia", "Greece",
                  "Hong Kong SAR", "Honduras", "Croatia", "Hungary",
                  "Indonesia", "Ireland", "Iran, Islamic Rep. of", "Iceland (Grade 5)", "Iceland", "Israel", "Italy",
                  "Kuwait",
                  "Lithuania", "Luxembourg", "Latvia",
                  "Morocco (6th Grade)", "Morocco", "Moldova, Republic of", "Macedonia, Rep. of", "Maltese - Malta", "Malta",
                  "Northern Ireland", "Netherlands", "Norway (Grade 5)", "Norway", "New Zealand",
                  "Oman",
                  "Poland", "Portugal",
                  "Qatar",
                  "Romania", "Russian Federation",
                  "Saudi Arabia", "Scotland", "Sweden (Grade 3)", "Singapore", "Slovak Republic", "Slovenia", "Sweden",
                  "Trinidad and Tobago", "Turkey", "Chinese Taipei",
                  "United States",
                  "South Africa"),
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