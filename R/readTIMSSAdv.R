#' @title Connect to TIMSS Advanced Data
#'
#' @description Opens a connection to a TIMSS Advanced data file and
#'              returns an \code{edsurvey.data.frame} with 
#'              information about the file and data.
#'
#' @param path a character value to the full directory to the TIMSS Advanced extracted SPSS (.sav) set of data.
#' @param countries a character vector of the country/countries to include using
#'                  the three-digit ISO country code.  
#'                  A list of country codes can be found on Wikipedia at
#'                  \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes},
#'                  or other online sources. Consult the \emph{TIMSS Advanced User Guide} to help determine what countries
#'                  are included within a specific testing year of TIMSS Advanced.
#'                  To select all countries, use a wildcard value of \strong{\code{*}}.
#' @param subject a character value to indicate if you wish to import the \code{math} or \code{physics} dataset. 
#'                Only one subject can be read in at a time.
#' @param forceReread a logical value to force rereading of all processed data. 
#'                    The default value of \code{FALSE} will speed up the \code{readTIMSSAdv} function by using existing read-in data already processed.
#'                    
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'
#' @details Reads in the unzipped files downloaded from the TIMSS Advanced international database(s) using the \href{http://rms.iea-dpc.org/}{IEA Study Data Repository}.
#'          Datafiles require the SPSS datafile (.sav) format using the default filenames.
#'
#' @details A TIMSSAdvanced \code{edsurvey.data.frame} includes three distinct data levels: 
#'          \itemize{
#'               \item student
#'               \item school
#'               \item teacher
#'          }
#'
#'          When the \code{getData} function is called using a TIMSS Advanced \code{edsurvey.data.frame},
#'          the requested data variables are inspected, and it handles any necessary data merges automatically. 
#'          Note that the \code{school} data will always be returned merged to the \code{student}
#'          data, even if only \code{school} variables are requested.
#'          Only if \code{teacher} variables are requested by the \code{getData} call, will cause the \code{teacher} data to be merged.
#'          Many \code{students} can be linked to many \code{teachers}, which varies widely between countries.
#'          
#' @details Please note that calling the \code{dim} function for a TIMSS Advanced \code{edsurvey.data.frame} will result in the row count as if the \code{teacher} dataset was merged.
#'          This row count will be considered the \code{full data N} of the \code{edsurvey.data.frame}, even if no \code{teacher} data were included in an analysis.  
#'          The column count returned by \code{dim} will be the count of unique column variables across all three data levels.
#'          
#' @return
#'  an \code{edsurvey.data.frame} for a single specified country or an \code{edsurvey.data.frame.list} if multiple countries specified
#'
#' @seealso \code{\link{readNAEP}}, \code{\link{readTIMSS}}, \code{\link{getData}}, and \code{\link{downloadTIMSSAdv}}
#' @author Tom Fink
#'
#' @example \man\examples\readTIMSSAdv.R
#'
#' @importFrom haven read_sav
#' @import tibble
#' @export
readTIMSSAdv <- function(path,
                      countries,
                      subject=c("math", "physics"),
                      forceReread=FALSE,
                      verbose=TRUE) {
  
  path <- normalizePath(unique(path), winslash = "/")
  subject <- tolower(subject)
  
  if(sum(!dir.exists(path)) > 0) { #validate the paths to ensure they all exist
    stop(paste0("Cannot find ", sQuote("path") , "value in ", paste(dQuote(path[!dir.exists(path)]), collapse=", ")))
  }
  
  if(!is.logical(forceReread)){
    stop(paste0("The argument ", sQuote("forceReread"), " must be a logical value."))
  }
  if(!is.logical(verbose)){
    stop(paste0("The argument ", sQuote("verbose"), " must be a logical value."))
  }
  if(length(subject)>1){
    stop(paste0("The argument ", sQuote("subject"), " must be of length 1."))
  }
  if(!(subject %in% c("math", "physics"))){
    stop(paste0("The argument ", sQuote("subject"), " must be a value of ", sQuote("math"), " or ", sQuote("physics"),"."))
  }
  
  #prepwork
  countries <- unique(tolower(countries))
  gradeLvl <- "Secondary" #TIMSS Advanced is for Students in their last year of secondary school
  subjChr <- ifelse(subject=="math", "m", "p") #determines the first character 
  
  if(unlist(countries)[1]=="*"){ #user has requested data for all countries::grab all country codes
    countries <- unique(tolower(substring(list.files(path, 
                                                     pattern=paste0("^", subjChr, ".....(",
                                                                    paste(getTIMSSAdvYearCodes(), collapse = "|"), ")\\.sav$"), full.names=FALSE, ignore.case = TRUE),4,6)))
  }
  
  #gather datafile listing::be sure we only pickup TIMSS Advanced years based on the yearcodes
  filenames <- list.files(path,
                          pattern=paste0("^", subjChr, "..", "(",paste(countries, collapse="|"), ")(",
                                         paste(getTIMSSAdvYearCodes(), collapse = "|"), ")","\\.sav$"), full.names=TRUE, ignore.case = TRUE)
  if(length(filenames) == 0) {
    stop(paste0("Could not find any TIMSS Advanced datafiles for countries: ", paste(countries, collapse=", "),
                " in the following folder(s): ", paste(path, collapse=", "), "."))
  }
  
  fSubPart <- tolower(substring(basename(filenames), 1, 8)) #includes a (4th grade), country code, and year code
  fileYrs <- sort(unique(tolower(substring(basename(filenames),7,8))))
  
  procCountryData <- list()
  iProcCountry <- 0 #index counter for adding countries to the list
  
  for(yrCode in fileYrs){ #loop through all the year codes first
    for(cntry in countries){
      
      TIMSSAdvfiles <- list()#empty list
      
      if(subject=="math"){
        TIMSSAdvfiles <- c("mcg", #school background
                           "msa", #student achievement
                           "msg", #student background
                           "msr", #student within-country scoring reliability
                           "mst", #student-teacher linkage
                           "mtg") #teacher background
      } else { #physics
        TIMSSAdvfiles <- c("pcg", #school background
                           "psa", #student achievement
                           "psg", #student background
                           "psr", #student within-country scoring reliability
                           "pst", #student-teacher linkage
                           "ptg") #teacher background
      }
      
      fnames <- NULL # be sure to clear this out so leftovers are not kept from a previous loop
      fnames <- sapply(TIMSSAdvfiles, function(f) {
        filenames[(fSubPart %in% paste0(f,cntry, yrCode))] #added check here to only grab files for our specific TIMSS grade level, country, and year
      }, simplify=FALSE)
      
      hasMissing <- sapply(fnames, function(g) {
        sum(nchar(g))==0
      }, simplify=TRUE)
      
      #There is no teacher linkage or teacher background data for TIMSS Advanced 1995
      hasMissing[paste0(subjChr, "sr")] <- FALSE
      hasMissing[paste0(subjChr, "st")] <- FALSE
      hasMissing[paste0(subjChr, "tg")] <- FALSE
      
      hasExcess <- sapply(fnames, function(h) {
        length(h)>1
      }, simplify=TRUE)
      
      #test for any missing files other than the 'ash' or 'asr' file::also check for any duplicate or multiple files
      if (sum(hasMissing)>0 && sum(nchar(unlist(fnames)))>0) {
        stop(paste0("Missing TIMSS Advanced Datafile(s) for Country (", cntry, "): ", paste(TIMSSAdvfiles[hasMissing], collapse=", ")))
      }
      if (sum(hasExcess)>0 && sum(nchar(unlist(fnames)))>0){
        stop(paste0("Excess/Duplicate TIMSS Advanced Datafile(s) for Country (", cntry, "): ", paste(TIMSSAdvfiles[hasExcess], collapse=", ")))
      }
      
      #test if there are any files for this country/year combination, if not, we can skip this loop iteration as it does not exist
      if (sum(nchar(unlist(fnames)))==0) {
        warning(paste0("No Data Found. Skipping Country ", sQuote(cntry), " for year ", sQuote(convertTIMSSAdvYearCode(yrCode))))
        next
      }
      
      iProcCountry <- iProcCountry + 1 #update the processed country index value after we confirm that there is data to process
      processedData <- list()
      
      processArgs <- list(dataFolderPath = path, 
                          countryCode = cntry, 
                          fnames = fnames, 
                          fileYrs = yrCode, 
                          subject = subject,
                          forceReread = forceReread, 
                          verbose = verbose)
      
      retryProc <- tryCatch({processedData <- do.call("processTIMSSAdv", processArgs, quote = TRUE)
                              FALSE
                            }, error = function(e){
                              TRUE #flag to retry
                            }, warning = function(w){
                              TRUE #flag to retry
                            })
      
      if (retryProc){
        processArgs[["forceReread"]] <- TRUE #try it again reprocessing the data
        processedData <- tryCatch(do.call("processTIMSSAdv", processArgs, quote = TRUE),
                                  error = function(e){
                                    stop(paste0("Unable to process TIMSS Advanced data for country code ", sQuote(cntry), 
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
      
      wgts <- tolower(subset(processedData$dataListFF$student, weights, "variableName"))
      
      #build the list of 'weights' looping through the weights we expect to be there and create our master list
      testJKprefix <- c("JK", "JK.MATWGT", "JK.PHYWGT") #have any jk prefix values here that are applicable for this dataset
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
          if(testJKprefix[i]=="JK.MATWGT"){
            tmpWgt <- list()
            tmpWgt[[1]] <- list(jkbase="jk.matwgt", jksuffixes=as.character(ujkz))
            names(tmpWgt)[[1]] <- "matwgt"
            weights <- c(weights,tmpWgt)
          }
          if(testJKprefix[i]=="JK.PHYWGT"){
            tmpWgt <- list()
            tmpWgt[[1]] <- list(jkbase="jk.phywgt", jksuffixes=as.character(ujkz))
            names(tmpWgt)[[1]] <- "phywgt"
            weights <- c(weights,tmpWgt)
          }
        }
      }
      attr(weights, "default") <- "totwgt"
      processedData$weights <-  weights
      
      if(subject=="math"){
        processedData$pvvars <- buildPVVARS_TIMSSAdv(processedData$dataListFF$student, defaultPV = "mmat")
        processedData$subject <- "Mathematics"
      }else{#if not math::then it's physics
        processedData$pvvars <- buildPVVARS_TIMSSAdv(processedData$dataListFF$student, defaultPV = "pphy")
        processedData$subject <- "Physics"
      }
      
      processedData$year <- convertTIMSSAdvYearCode(yrCode)
      processedData$assessmentCode <- "International"
      processedData$dataType <- "Student Data"
      processedData$gradeLevel <- "Secondary"
      
      processedData$achievementLevels <- c("625", "550", "475") #only 3 levels of achievement for TIMSS Advanced
      names(processedData$achievementLevels) <- c("Advanced International Benchmark", "High International Benchmark", "Intermediate International Benchmark")
      
      processedData$omittedLevels <- c('Multiple', NA, 'NOT ADMINISTERED', 'OMITTED', 'OMITTED OR INVALID', 'NOT REACHED', 'TWO OR MORE RESPONSES, UNINTERPRETABLE','LOGICALLY NOT APPLICABLE', 'MISSING', 'MISSING (BLANK ONLY)', '(Missing)')

      processedData$fileFormat <- processedData$dataListFF$student
      processedData$fileFormatSchool <- processedData$dataListFF$school
      processedData$fileFormatTeacher <- processedData$dataListFF$teacher
      processedData$survey <- "TIMSS Advanced"
      processedData$country <- getTIMSSAdvCountryName(cntry)
      
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

#@param yrCode a character value used in the TIMSS Advanced filenaming structure to identify the specific year (e.g. m1, m2, m6)
#@return a numeric 4 digit year value
convertTIMSSAdvYearCode <- function(yrCode){
  
  yrTest <- tolower(sort(unique(yrCode)))
  yrTest[yrTest %in% c("m1")] <- 1995
  yrTest[yrTest %in% c("m2")] <- 2008
  yrTest[yrTest %in% c("m3")] <- 2015
  
  return(yrTest)
}

getTIMSSAdvYearCodes <- function(){
  #retrieve the TIMMS years based on their filenaming structure
  
  yrVals = c("m1","m2","m3")
  names(yrVals) = c(1995, 2008, 2015)

  return(yrVals)
}

#builds the list of pvvars from the passed fileformat data.frame
buildPVVARS_TIMSSAdv <- function(fileFormat, defaultPV = "mmat"){
  
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


#@param dataFolderPath a character value of the initial folder path provided to the 'readTIMSSAdv' call to find the  .sav SPSS files
#@param countryCode a character value of the 3-digit country code we want to process
#@param fnames a character vector of the specific filenames that are needed for this country, generally there should be 7 files specified
processTIMSSAdv <- function(dataFolderPath, countryCode, fnames, fileYrs, subject, forceReread, verbose) {
  
  yearCode <- unlist(fileYrs)[1]
  subjChr <- ifelse(subject=="math", "m", "p") #get the first filename char based on the subject
  
  metaCacheFP <- list.files(dataFolderPath,
                            pattern=paste0("^", subjChr, "(",paste(countryCode), ")",
                                           yearCode, "\\.meta$"), full.names=TRUE, ignore.case = TRUE)
  
  #grab the FWF .txt files for this country/year if they are existing
  txtCacheFWF <- list.files(dataFolderPath,
                            pattern=paste0("^", subjChr,"..", "(",paste(countryCode), ")",
                                           yearCode, "\\.txt$"), full.names=TRUE, ignore.case = TRUE)
  
  #determine if we can use the .meta RDS file for reading, OR process the data and create the .meta RDS
  if(length(metaCacheFP)==0 || (length(txtCacheFWF)<3 && fileYrs!="m1" || (length(txtCacheFWF)<2 && fileYrs=="m1")) || forceReread==TRUE){ #ensure we have a full dataset of cache files:: The 1995 dataset only had school and student level data
    runProcessing <- TRUE
  }else{
    cacheFile <- readRDS(unlist(metaCacheFP)[1])
    
    if (cacheMetaReqUpdate(cacheFile$cacheFileVer, "TIMSS Advanced")){ #cacheMetaReqUpdates resides in its own R file
      runProcessing <- TRUE
    }else{
      #rebuild the file connections from the .meta serialized cache file using the stored fileFormats
      studentLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))==paste0(subjChr, "sg")], cacheFile$dataListFF$student)
      schoolLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))==paste0(subjChr, "cg")], cacheFile$dataListFF$school)
      
      if(any(names(cacheFile$dataListFF) %in% "teacher")){ #1995 dataset had no teacher data
        teacherLAF <- getFWFLaFConnection(txtCacheFWF[tolower(substr(basename(txtCacheFWF),1,3))==paste0(subjChr, "tg")], cacheFile$dataListFF$teacher)
        dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
      } else {
        dataList <- list(student = studentLAF, school = schoolLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
      }
      
      dataListFF <- cacheFile$dataListFF
      dataListMeta <- cacheFile$dataListMeta
      
      runProcessing <- FALSE
    }
  } #end if(length(metaCacheFP)==0 || (length(txtCacheFWF)<3 && fileYrs!="m1" || (length(txtCacheFWF)<2 && fileYrs=="m1")) || forceReread==TRUE)

  
  if(runProcessing==TRUE){
    
    if(verbose==TRUE){
      cat(paste0("Processing Data for Country: ", dQuote(countryCode),"\n"))
    }
    
    #SCHOOL LEVEL===================================================
    cg <- unlist(fnames[paste0(subjChr, "cg")])[1]
    schoolFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(subjChr, "cg")])[1], ignore.case = TRUE)
    schoolDF1 <- read_sav(cg)
    colnames(schoolDF1) <- toupper(colnames(schoolDF1))
    ffsch <- writeTibbleToFWFReturnFileFormat(schoolDF1, schoolFP )  
    #===============================================================
    
    #STUDENT LEVEL==================================================
    sa <- unlist(fnames[paste0(subjChr, "sa")])[1]
    sg <- unlist(fnames[paste0(subjChr, "sg")])[1]
    sr <- unlist(fnames[paste0(subjChr, "sr")])[1]
    stuDF1 <- read_sav(sa)
    colnames(stuDF1) <- toupper(colnames(stuDF1))
    ids1 <- grep("^ID", names(stuDF1), ignore.case=TRUE, value=TRUE)
    stuDF2 <- read_sav(sg)
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
      stop(paste0("Failed consistency check for filetype ", sQuote(paste0(subjChr, "sa")), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }
    if(nrow(stuDF2) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote(paste0(subjChr, "sg")), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }
    
    #test we have the sr file before merging
    if(min(is.na(sr)) == 0) {
      stuDF3 <- read_sav(sr)
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
        stop(paste0("Failed consistency check for filetype ", sQuote(paste0(subjChr, "sr")), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance"))
      }
      if(nr != nrow(mm)) {
        stop(paste0("Failed consistency check for filetype ", sQuote(paste0(subjChr, "sr")), " country code ", sQuote(tolower(countryCode)), ". ",
                    "Please email EdSurvey.help@air.org for assistance"))
      }
    } else {
      idsmm3 <- ids12
    }
    
    stuFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(subjChr, "sg")])[1], ignore.case = TRUE)
    ffstu <- writeTibbleToFWFReturnFileFormat(mm, stuFP)  
    #===============================================================
    
    #Student-Teacher Linkage and Teacher Background=================
    st <- unlist(fnames[paste0(subjChr, "st")])[1]
    tg <- unlist(fnames[paste0(subjChr, "tg")])[1]
    
    #ensure both the st and tg files are available before merging
    if(min(is.na(st))==0 && min(is.na(tg))==0){
    stuTeachDF <- read_sav(st)
    colnames(stuTeachDF) <- toupper(colnames(stuTeachDF))
    teachDF <- read_sav(tg)
    colnames(teachDF) <- toupper(colnames(teachDF))
    
    ids1 <- grep("^ID", names(stuTeachDF), ignore.case=TRUE, value=TRUE)
    ids2 <- grep("^ID", names(teachDF), ignore.case=TRUE, value=TRUE)
    ids12 <- ids1[ids1 %in% ids2]
    ids12 <- ids12[!(ids12 %in% c("IDPUNCH", "IDGRADER"))] #IDPUNCH should be omitted for merging
    
    mm <- mergeTibble(stuTeachDF,
                      teachDF,
                      by=ids12,
                      all.x=TRUE,
                      all.y=FALSE,
                      suffixes=c("", ".junk"))
    mm <- mm[,names(mm)[!grepl("\\.junk$",names(mm))]]
    
    if(nrow(stuTeachDF) != nrow(mm)) {
      stop(paste0("Failed consistency check for filetype ", sQuote(paste0(subjChr, "tg")), " country code ", sQuote(tolower(countryCode)), ". ",
                  "Please email EdSurvey.help@air.org for assistance"))
    }
    
    teachFP <- gsub(".sav$", "\\.txt", unlist(fnames[paste0(subjChr, "tg")])[1], ignore.case = TRUE)
    ffTeach <- writeTibbleToFWFReturnFileFormat(mm, teachFP)
    
    teacherLAF <- getFWFLaFConnection(teachFP, ffTeach)
    schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
    studentLAF <- getFWFLaFConnection(stuFP, ffstu)
    
    #build data list and link metadata object=======================
    dataList <- list(student = studentLAF, school = schoolLAF, teacher = teacherLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
    dataListFF <- list(student = ffstu, school = ffsch, teacher = ffTeach)
    
    dataListMeta <- list(student = "", school = "", teacher = "")
    dataListMeta$student <- list(school = "idcntry;idschool", teacher = "idcntry;idstud") #idschool #do we need to merge on idclass var as well?
    dataListMeta$school <- list()
    dataListMeta$teacher <- list()
    #===============================================================
    
    } else { #1995 TIMSS Advanced data had no teacher-student linkage or background file:: so we need to omit those
      
      schoolLAF <- getFWFLaFConnection(schoolFP, ffsch)
      studentLAF <- getFWFLaFConnection(stuFP, ffstu)
      
      #build data list and link metadata object=======================
      dataList <- list(student = studentLAF, school = schoolLAF) #ORDER THE dataList in a heirarchy, ie. student list should be first
      dataListFF <- list(student = ffstu, school = ffsch)
      
      dataListMeta <- list(student = "", school = "", teacher = "")
      dataListMeta$student <- list(school = "idcntry;idschool")
      dataListMeta$school <- list()
    }
    #===============================================================
    
    
    #save the cachefile to be read-in for the next call
    cacheFile <- list(ver=packageVersion("EdSurvey"),
                      cacheFileVer=2,
                      ts=Sys.time(),
                      dataListFF=dataListFF,
                      dataListMeta=dataListMeta)
    
    saveRDS(cacheFile, file.path(dataFolderPath,paste0(subjChr, countryCode, yearCode,".meta")))
    
  } else { #used the cache files
    if(verbose==TRUE){
      cat(paste0("Found cached data for country code ", dQuote(countryCode),"\n"))
    }
  } #end if(runProcessing==TRUE)
  
  return(list(dataList = dataList,
              dataListFF = dataListFF,
              dataListMeta = dataListMeta)) 
}


exportTIMSSAdvToCSV <- function(folderPath, exportPath, cntryCodes, subject, ...){
  
  sdfList <- readTIMSSAdv(folderPath, cntryCodes, subject, ...)
  
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
getTIMSSAdvCountryName <- function(countryCode){
  
  cntryCodeDF <- data.frame(
    cntryCode = c("arm", "aus", "aut", 
                  "can", "che", "cyp", "cze",
                  "deu", "dnk",
                  "fra",
                  "grc",
                  "irn", "isr", "ita",
                  "lbn", "ltu", "lva",
                  "nld", "nor",
                  "phl", "prt",
                  "rtr", "rus",
                  "svn", "swe",
                  "usa"),
    cntryName = c("Armenia", "Australia", "Austria",
                  "Canada", "Switzerland", "Cyprus", "Czech Republic",
                  "Germany", "Denmark",
                  "France",
                  "Greece",
                  "Iran, Islamic Republic of", "Israel", "Italy",
                  "Lebanon", "Lithuania", "Latvia",
                  "Netherlands", "Norway",
                  "Philippines", "Portugal",
                  "Russian Federation 6hr+", "Russian Federation",
                  "Slovenia", "Sweden",
                  "United States"),
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