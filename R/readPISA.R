#' @title Connect to PISA Data
#'
#' @description Opens a connection to a PISA data file and
#'              returns an \code{edsurvey.data.frame} with
#'              information about the file and data.
#'
#' @param path a character vector to the full directory path(s) to the PISA-extracted fixed-width files
#'        and SPSS control files (.txt).
#' @param database a character to indicate a selected database. Must be one of \code{INT} (general database that most people use), \code{CBA} (computer-based database in PISA 2012 only), or \code{FIN} (financial literacy database in PISA 2012 only).
#'        Defaults to \code{INT}.
#' @param countries a character vector of the country/countries to include using the
#'        three-digit ISO country code. A list of country codes can be found in the PISA codebook or \url{https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes}.
#'        If files are downloaded using \code{\link{downloadPISA}}, a country dictionary text file can be
#'        found in the filepath.
#' @param cognitive one of \code{none}, \code{score}, or \code{response}. Default is \code{score}. The PISA database often has three student files: student questionnaire, cognitive item
#'        response, and scored cognitive item response. The first file is used as the main student file with student background information. Users can choose whether to merge \code{score} or
#'        \code{response} data into the main file or not (if \code{none}).
#' @param forceReread a logical value to force rereading of all processed data. Defaults to \code{FALSE}.
#'        Setting \code{forceReread} to be \code{TRUE} will cause PISA data to be reread and increase processing time.
#' @param verbose a logical value that will determine if you want verbose output while the function is running to indicate progress.
#'        Defaults to \code{TRUE}.
#' @details Reads in the unzipped files downloaded from the PISA database using the OECD Repository (\url{http://www.oecd.org/pisa/}). Users can use \code{\link{downloadPISA}} to
#'        download all required files.
#'        Student questionnaire files (with weights and plausible values) are used as main files, which are then
#'        merged with cognitive, school, and parent files (if available).
#'
#'         The average first-time processing time for 1 year and one database for all countries is 10--15 minutes. If \code{forceReread} is set
#'         to be \code{FALSE}, the next time this function is called will only take 5--10 seconds.
#'
#' @return
#'  an \code{edsurvey.data.frame} for a single specified country or
#'   an \code{edsurvey.data.frame.list} if multiple countries are specified
#' @seealso \code{\link{getData}} and \code{\link{downloadPISA}}
#' @author Trang Nguyen
#'
#' @example man/examples/readPISA.R
#'
#' @references
#'  OECD. (2017). \emph{PISA 2015 technical report}. Paris, France: OECD Publishing. Retrieved from \emph{\url{http://www.oecd.org/pisa/data/2015-technical-report/}}
#'
#' @importFrom data.table fread fwrite
#' @importFrom readr read_csv
#' @importFrom readr read_lines
#' @importFrom stringi stri_sub
#' @export
readPISA <- function(path,
                     database = "INT",
                     countries,
                     cognitive = "score",
                     forceReread = FALSE,
                     verbose = TRUE) {
  # Check that the arguments path, database, cognitive, forceReread, and verbose are valid  ======
  # database must be length one and upper, so force that
  database <- toupper(database[[1]])
  if(!database %in% c("INT", "CBA", "FIN")) {
    stop("The argument ", sQuote("database"), " must be one of ", sQuote("INT"), ", ", sQuote("CBA"), ", or ", sQuote("FIN"), ".")
  }
  if (!is.character(countries)) {
    stop("The argument ",sQuote('countries'), " must be a character vector.")
  }
  if (!cognitive %in% c("score", "none", "response")) {
    stop("The argument ", sQuote("cognitive"), " must be one of ", sQuote("none"), ", ", sQuote("score"), ", or ", sQuote("response"), ".")
  }
  if (!is.logical(verbose)) {
    stop("The argument ",sQuote("verbose")," must be a logical value.")
  }
  if (!is.logical(forceReread)) {
    stop("The argument ",sQuote("forceReread")," must be a logical value.")
  }

  path <- normalizePath(path, winslash = "/") # to match IEA read-in function
  if(!all(dir.exists(path))){
    stop(paste0("The argument ", sQuote("path"), " cannot be located ", pasteItems(path[!dir.exists(path)]),"."))
  }

  # Gather file names to be read in ====
  databasecode <- ifelse(database == "INT","",paste0(database,"_"))
  sdf <- list() # list to contain all edsurvey.data.frame elements
  icntry <- 0 # index of each edsurvey.data.frame in the list
  for (filepath in path) { # loop through different data paths
    # check to see whether it's 2015 data
    if (length(list.files(filepath, pattern="cy6.*\\.sav", ignore.case=TRUE, full.names=FALSE)) > 0) {
      year <- 2015
      database <- "INT" # in PISA 2015, financial literacy and problem solving can be merged back to the overall population
      # check for meta file
      runProcessing <- FALSE
      metaCacheFile <- list.files(filepath, pattern="\\.meta$", ignore.case = TRUE)
      if (length(metaCacheFile) < 0 | forceReread) {
        runProcessing <- TRUE
      } else {
        cacheFile <- tryCatch(readRDS(file.path(filepath,metaCacheFile[1])),
                              warning = function(w) {
                                runProcessing <<- TRUE
                                cat(sQuote(metaCacheFile[1]), " is a corrupt file. Reprocessing PISA files", "\n")
                                return(NULL)
                              },
                              error = function(err) {
                                runProcessing <<- TRUE
                                cat(sQuote(metaCacheFile[1]), " is a corrupt file. Reprocessing PISA files", "\n")
                                return(NULL)
                              })
        if (!is.null(cacheFile)) {
          if(cacheMetaReqUpdate(cacheFile$cacheFileVer, "PISA")) {
            runProcessing <- "TRUE"
            cat("Cache files are outdated. Reprocessing PISA files ... \n")
          }
        }
      }
      if (runProcessing) {
        cacheFile <- processPISA2015(filepath, verbose, countries)
      }
      ff <- list(fileFormat = cacheFile$dict)
      all_countries <- cacheFile$countryDict$cnt[cacheFile$countryDict$available]
      if (countries == "*") {
        countries <- all_countries
      }
      countries <- tolower(countries)
      processedValue <- list(datbasename = "M_DAT_CY6_MS_CMB_STU",
                             countries = countries)
    } else {
      if (database == "INT") {
        controlFilenames <- list.files(filepath, pattern = "SPSS_[a-z].*\\.txt", ignore.case = FALSE, full.names = FALSE)
      } else {
        controlFilenames <- list.files(filepath, pattern = paste0("SPSS_",database,".*\\.txt"), ignore.case = FALSE, full.names = FALSE)
      }
      if (length(controlFilenames) == 0) {
        stop("Missing PISA Datafile(s) (",database," database) in the path ",sQuote(path))
      }

      year <- unique(as.numeric(gsub("PISA","",strsplit(controlFilenames,"_")[[1]][1])))

      # validate data
      if (length(year) == 0) {
        stop("Please make sure to download SPSS syntax files on the OECD website. You can use ",sQuote('downloadPISA')," to download and organize data.")
      }

      if (length(year) > 1) {
        stop("Found more than 2 years of data in the folder. Please separate different years of data in different folders.")
      }
      # files different for each year
      if (year %in% c(2006,2009,2012)) {
        filebasenames <- c("student","cognitive","score", "parent", "school")
      } else if (year == 2000) {
        filebasenames <- c("student_reading", "student_science","student_math", "cognitive","school")
      } else if (year == 2003) {
        filebasenames <- c("student","cognitive","school")
      } else if (year == 2015) {
        # Use Sav files
        filebasenames <- c("stu_qqq","stu_cog","stu_flt","stu_cps","stu_qtm","sch_qqq")
      } else {
        stop(sQuote(year)," is not a valid year. Valid years for PISA are 2000, 2003, 2006, 2009, 2012, and 2015.")
      }

      # controlFilenames is a vector of all SPSS syntax files
      # now we need to attach each SPSS syntax file to its right label (one of `filebasenames` above)
      labelFiles <- c()
      for (i in 1:length(controlFilenames)) {
        labelFiles[i] <- filebasenames[which(sapply(filebasenames,function(x) {
          # we need to match against the whole string i.e. SPSS_student because cognitive files
          # might be in SPSS_cognitvie_student which also has the word "student"
          grepl(paste0("SPSS_",databasecode,x),controlFilenames[i], ignore.case = TRUE)
        }))]
      }
      # controlFileNames now is a named vector that contains all SPSS syntax files we will process
      # names of the vector definie file type (i.e. student or school type)
      names(controlFilenames) <- labelFiles

      if (all(!grepl("student",labelFiles))) {
        stop("Missing student SPSS syntax file. Since student is the main level, it is required that there must be at least one student file in the directory.")
      }
      # rearrange to make sure student files are the main ones
      labelFiles <- unique(c(grep("student",labelFiles,value=TRUE),labelFiles))

      # filter out some unnecessary files
      if (cognitive == "none") {
        labelFiles <- labelFiles[!labelFiles %in% c("score","cognitive")]
      } else if (cognitive == "response") {
        labelFiles <- labelFiles[!labelFiles %in% c("score")]
        if (year %in% c(2000,2003)) {
          cognitive <- "cognitive"
        }
      } else if (cognitive == "score") {
        if (!year %in% c(2000,2003)) {
          labelFiles <- labelFiles[!labelFiles %in% c("cognitive")]
        } else {
          cognitive <- "cognitive"
        }
      }
      controlFilenames <- controlFilenames[labelFiles]
      controlFilenames <- file.path(filepath, controlFilenames)


      # Process control files for each database ======================================
      # Return a list of name of data file and dict table
      metaCacheFile <- gsub("\\.txt","\\.meta",controlFilenames)
      masterData <- lapply(controlFilenames, function(x) {
        metaCacheFile <- gsub("\\.txt","\\.meta",x)
        if(!file.exists(metaCacheFile) || forceReread) {
          if (verbose) {
            cat("Processing SPSS control file ", sQuote(x), "\n")
          }
          return(suppressWarnings(readDict(x)))
        }
        cacheFile <- tryCatch(readRDS(metaCacheFile),
                              warning = function(w) {
                                forceReread <<- TRUE
                                cat(sQuote(x), " is a corrupt file. Reprocessing PISA files", "\n")
                                return(suppressWarnings(readDict(x)))
                              },
                              error = function(err) {
                                forceReread <<- TRUE
                                cat(sQuote(x), " is a corrupt file. Reprocessing PISA files", "\n")
                                return(suppressWarnings(readDict(x)))
                              })
        if(cacheMetaReqUpdate(cacheFile$cacheFileVer, "PISA")) {
          if (verbose) {
            cat("Processing SPSS control file ", sQuote(x), "\n")
          }
          return(suppressWarnings(readDict(x)))
        } else {
          return(cacheFile)
        }
      })
      names(masterData) <- labelFiles
      # Setting key variables for linkage with student data
      masterData <- lapply(masterData, function(l) {
        l$linkid <- intersect(c("cnt","schoolid","stidstd"),tolower(l$dict$variableName))
        return(l)
      })

      # Reading the merged FF format file
      # ff is a list with: FFname = name of the FF file (to be used for base file name)
      #                    fileFormat = dataframe with all merged variables
      LafDictList = masterData
      masterData <- NULL

      mergeFFmeta <- list.files(filepath, pattern = paste0("^M_FF_.*",database,".*",cognitive,".*\\.meta"), full.names = FALSE, ignore.case = TRUE)
      if (length(mergeFFmeta) == 0 || forceReread) {
        # Write merged files
        # ff is a list that contains:
        # 1. FFname = file name of the meta processed file format file (as followed)
        # 2. fileFormat = data.frame that includes information on all variables from student questionaire
        # school, and (possibly) parent files
        ff <- mergeFF(filepath = filepath, LafDictList = LafDictList,
                      by = lapply(LafDictList[2:length(LafDictList)], function(l) { return(l$by)}),
                      mergeSuffixes = cognitive)
      } else {
        for (i in 1:length(mergeFFmeta)) {
          ff <- tryCatch(readRDS(file.path(filepath, mergeFFmeta[i])),
                         error = function(err) {
                           cat(err,". Reprocessing the meta file. \n")
                           return(mergeFF(filepath = filepath, LafDictList = LafDictList,
                                          by = lapply(LafDictList[2:length(LafDictList)], function(l) { return(l$by)}),
                                          mergeSuffixes = cognitive))
                         },
                         warning = function(w) {
                           cat(w, "Reprocessing the meta file \n")
                           return(mergeFF(filepath = filepath, LafDictList = LafDictList,
                                          by = lapply(LafDictList[2:length(LafDictList)], function(l) { return(l$by)}),
                                          mergeSuffixes = cognitive))
                         })
          if (cacheMetaReqUpdate(ff$cacheFileVer, "PISA")) {
            if (i < length(mergeFFmeta)) {
              next
            }
            ff <- mergeFF(filepath = filepath, LafDictList = LafDictList,
                          by = lapply(LafDictList[2:length(LafDictList)], function(l) { return(l$by)}),
                          mergeSuffixes = cognitive)
          } else {
            break
          }
        }
      }
      # Process merge txt data file ====================================
      processedValue <- processMergeTxt(filepath, LafDictList, countries, ff, database, forceReread, verbose)
    }

    # From this step, requires:
    # (1) ff: a data.frame of all merged variables
    # (2) year
    # (3) database
    # (4) processedValue: (a) datbasename and (b) countries

    # Set up weights =================================================================
    jksuffix <- gsub("[^0-9]", "",ff$fileFormat$variableName[grepl("w_.*[0-9]$", ff$fileFormat$variableName,
                                                                   ignore.case = TRUE)])
    jksuffix <- unique(jksuffix)
    weight_var <- grep("w_fstuwt",tolower(ff$fileFormat$variableName), ignore.case = TRUE, value = TRUE)
    weights <- list()
    for (w in weight_var) {
      if (year == 2015) {
        weights[[w]] <- list(jkbase = "w_fsturwt", jksuffixes = jksuffix)
      } else {
        weights[[w]] <- list(jkbase = paste0("w_fstr",gsub("w_fstuwt","",w)), jksuffixes = jksuffix)
      }
    }
    attr(weights, "default") <- names(weights)[1]

    # achievementLevels
    suppressWarnings(achievement <- pisaAchievementHelp(year, database[1]))

    # Set up PVS ======================================================================
    pvs = list()
    pv_subset <- subset(ff$fileFormat, select = c('Type', 'variableName'),
                        ff$fileFormat$Type != "")
    uniquePvTypes = tolower(unique(pv_subset$Type))
    default_list <- c()
    for (i in uniquePvTypes) {
      vars <- tolower(pv_subset$variableName[tolower(pv_subset$Type) == i])
      temp_list <- list(varnames = vars)
      checkregex <- sapply(achievement$regex, grepl, i, ignore.case = T)
      subject <- achievement$subjects[which(checkregex)]
      if(!is.null(subject) && length(subject) > 0) {
        temp_list$achievementLevel <- sort(achievement$achievementLevels[[subject]])
        if (any(subject %in% achievement$default)) {
          default_list <- c(default_list,i)
        }
      } else {
        temp_list$achievementLevel <- NULL
      }
      pvs[[i]] <- temp_list
    } #end for (i in uniquePvTypes)

    if (length(default_list) > 0) {
      attr(pvs, "default") <- default_list[1]
    } else {
      attr(pvs, "default") <- names(pvs)[1]
    }

    # Read text files and return output ===========================
    if (year != 2015) {
      countryDict <- read.csv(file.path(filepath,paste0(database,"_all-countries.txt")),stringsAsFactors = F)
    } else {
      countryDict <- cacheFile$countryDict
    }
    all_countries <- countryDict$cnt[countryDict$available]
    for (cntry in tolower(processedValue$countries)) {
      if(verbose) {
        if (cntry %in% tolower(all_countries)) {
          cat("Found cached data for country code ")
          cat(dQuote(cntry))
          cat("\n")
        } else {
          cat("Data for country code",dQuote(cntry),"is not available for PISA", year,"\n")
        }
      }
      datLaf <- catchCountryTxt(filepath, datname = paste0(processedValue$datbasename, "_",cntry,".txt"),ff)
      icntry <- icntry + 1
      sdf[[icntry]] <- edsurvey.data.frame(userConditions = list(),
                                          defaultConditions = NULL,
                                          dataList = buildPISA_dataList(datLaf, ff$fileFormat),
                                          weights = weights,
                                          pvvars = pvs,
                                          subject = achievement$subjects,
                                          year = as.character(year),
                                          assessmentCode = "International",
                                          dataType = "Student Data",
                                          gradeLevel = "15 years old or above",
                                          achievementLevels = achievement$achievementLevels,
                                          omittedLevels = c("Invalid","N/A","Missing","Miss",NA,"(Missing)","NO RESPONSE","INVALID","VALID SKIP","NOT APPLICABLE","MISSING","NOT REACHED"),
                                          survey = "PISA",
                                          psuVar = "var_unit",
                                          stratumVar = "wvarstrr",
                                          jkSumMultiplier = 0.05, # this number is from PISA 2015 Technical Report Chapter 8 (in reference)
                                          country = convertCountryName(countryDict,cntry),
                                          validateFactorLabels = TRUE,
                                          reqDecimalConversion = FALSE)
    }
  } # end for(filepath in path)
  # Return output
  if (length(sdf) == 1) {
    return(sdf[[1]])
  } else {
    return(edsurvey.data.frame.list(sdf))
  }
}

# HELPER FUNCTION ========================================
# Used when .txt files for data and SPSS controller are provided
# @return: a list of
#   dat = data file name
#   dict = a data.frame that stores fwf information
readDict <- function(filename) {
  # Expected outcome
  dict <- data.frame("variableName" = character(0),
                     "Start" = integer(0),
                     "End" = integer(0),
                     "Width" = integer(0),
                     "Decimal" = integer(0),
                     "Labels" = list(),
                     "labelValues" = character(0),
                     "Type" = character(0),
                     "pvWt" = character(0),
                     "dataType" = character(0),
                     "weights" = character(0),
                     stringsAsFactors = FALSE)

  # Read in spss control files
  controlFile <- readr::read_lines(filename, progress = FALSE)
  controlFile <- gsub("\t"," \t", controlFile)
  controlFile <- gsub("[^[:print:]]","", controlFile) #remove unprintable characters
  controlFile <- trimws(controlFile,which = "both") #remove leading or ending whitespace
  # Note: the following lines need to be in order
  # Some syntax files uses single quote instead of double quote
  # Because later code use quote as a pattern to get labelValues, it's necessary
  # to replace relevant single quotes with double quotes
  controlFile <- gsub(" \'"," \"", controlFile) #replace single quote with double quote for later use
  controlFile <- gsub("\' ","\" ", controlFile)
  controlFile <- gsub("^\'|\'$","\"", controlFile)
  controlFile <- gsub("\'/","\"/", controlFile)
  controlFile <- gsub("\'\\.","\"\\.", controlFile)
  # end Note

  if (grepl("2003", filename)) {
    controlFile <- gsub(" / "," /\n ", controlFile)
    controlFile <- unlist(strsplit(controlFile,"\n "))
  }
  controlFile <- gsub("\\(40\" ","\\(40 ", controlFile)
  controlFile <- controlFile[controlFile != ""]
  # Get data file name
  i <- 1
  while (!grepl("file.*C:", controlFile[i], ignore.case = TRUE)) {
    i <- i+1
  }
  # Some syntax files uses single quote instead of double quote. For the sake of convenience,
  # change all of them to double quote
  controlFile[i] <- gsub("\'","\"",controlFile[i])

  # Find the name of the corresponding flat file given the SPSS syntax file
  datFname <- gsub("\"","",strsplit(stringi::stri_extract_all_regex(controlFile[i],'(?<=").*?(?=")"')[[1]],
                                    split = "\\\\")[[1]][3])
  # Some error in SPSS control file in 2009
  datFname <- gsub("PAQ09","PAR09", datFname)
  datFname <- gsub("INT_cogn_2003.txt","INT_cogn_2003_v2.txt", datFname)

  # Get variable name, fixed width and data types
  i <- i+1
  # This while loop will only look at the variable name and fixed width section
  # of the SPSS syntax file
  while (grepl("-", controlFile[i])) {
    # The next five lines remove some of leading symbols to avoid lack
    # of consistency between SPSS syntax files of different years
    # These are based on trial and error, and manually looking at different syntax files
    controlFile[i] <- gsub("^/","", controlFile[i])
    controlFile[i] <- gsub("\\("," \\(", controlFile[i])
    controlFile[i] <- gsub("-"," - ", controlFile[i])
    tempSplit <- unlist(strsplit(controlFile[i]," |\t"))
    tempSplit <- tempSplit[tempSplit != ""]
    # Exception: in PISA 2006, format of control file is different
    if (length(tempSplit) <= 3) {
      temp <- unlist(strsplit(tempSplit[2],"-"))
      tempDict <- data.frame("variableName" = toupper(tempSplit[1]),
                             "Start" = as.integer(temp[1]),
                             "End" = as.integer(temp[2]), stringsAsFactors = FALSE)
      if(length(tempSplit) == 2) {
        tempDict$Decimal <- 0
        tempDict$dataType <- "integer"
      } else {
        tempDict$Decimal <- NA
        tempDict$dataType <- "character"
      }
    } else {
      tempDict <- data.frame("variableName" = toupper(tempSplit[1]),
                             "Start" = as.integer(tempSplit[2]),
                             "End" = as.integer(tempSplit[4]), stringsAsFactors = FALSE)
      tempType <- gsub("\\(|\\)", "",tempSplit[5])
      if (grepl("^a", tempType,ignore.case = T)) {
        tempDict$Decimal <- NA
        tempDict$dataType <- "character"
      } else if (grepl("^f", tempType,ignore.case = T)){
        tempDict$Decimal <- as.integer(unlist(strsplit(tempType,"\\,"))[2])
        tempDict$dataType <- ifelse(tempDict$Decimal == 0, "integer","numeric")
      } else {
        tempDict$Decimal <- NA
        tempDict$dataType <- NA
      }
    }
    tempDict$Width <- tempDict$End - tempDict$Start + 1
    i <- i+1
    dict <- rbind(dict, tempDict)
  } # end while (grepl("-", controlFile[i]))
  dict$Decimal[is.na(dict$Decimal)] <- 0
  # --- End getting variable names, fixed width and data types

  # Get variable formats
  j <- 1
  while(!grepl("^format",controlFile[j], ignore.case = TRUE) && j <= length(controlFile)) {
    j <- j+1
    }
  while(j <= length(controlFile) && !grepl("exe.*\\.$", controlFile[j], ignore.case = TRUE)) {
    if (grepl("miss|val", controlFile[j], ignore.case = TRUE)) {
      j <- j+1
      next #some SPSS files do not list all "format lines" in one place
    }
    controlFile[j] <- gsub("\\("," \\(", controlFile[j])
    formatTemp <- strsplit(controlFile[j], " ")[[1]]
    formatTemp <- formatTemp[formatTemp != ""]
    if (grepl("format",controlFile[j], ignore.case = TRUE)) {
      fvarname <- c()
    }
    fvarname <- c(fvarname,grep("(format|\\()", formatTemp, invert = TRUE, value = TRUE, ignore.case = T))
    index_of_to_format <- which(tolower(fvarname) == "to")
    if (length(index_of_to_format) != 0) {
      additional <- c()
      for (ii in index_of_to_format) {
        index_of_first <- which(dict$variableName == toupper(fvarname[ii - 1]))
        index_of_last <-  which(dict$variableName == toupper(fvarname[ii + 1]))
        additional <- c(additional, dict$variableName[seq(from = index_of_first, to = index_of_last, by = 1)])
      }
      fvarname <- c(fvarname, additional)
      fvarname <- fvarname[tolower(fvarname) != "to"]
    }
    if (any(grepl("\\(f", formatTemp, ignore.case = TRUE))) {
      decimal <- gsub("[^0-9]","",unlist(strsplit(grep("\\(f", formatTemp, ignore.case = T, value = T),"\\."))[2])
      dict$Decimal[dict$variableName %in% toupper(fvarname)] <- as.numeric(decimal)
      fvarname <- c()
      if (all(grepl("^format",controlFile[(j+1):length(controlFile)], ignore.case = TRUE) == FALSE)) {
        break
      }
    }
    j <- j+1
  } # end loop for reading format
  dict$dataType[is.na(dict$dataType)] <- ifelse(dict$Decimal[is.na(dict$dataType)] == 0, "integer","numeric")

  # --- End checking formats
  # This column 'multiplier' is created to be consistent with the columns in fileFormat for other datasets.
  # It is not used for PISA because PISA is written out to csv, not fwf.
  dict$multiplier <- as.integer(ifelse(is.na(dict$Decimal), 1, 10^dict$Decimal))


  # Get variable labels
  # In syntax file of 2009 and earlier, there are formats. Need to skip
  while(!grepl("(variable label|var lab)", controlFile[i], ignore.case = TRUE)) { i <- i+1}
  i <- i+1
  # Get Variable labels
  while (!grepl("^\\.", controlFile[i]) && !grepl("^exe.*\\.", controlFile[i], ignore.case = TRUE) && !grepl("\\.$",trimws(controlFile[i-1],"right"))) {
    tempSplit <- strsplit(controlFile[i], "\"")[[1]]
    dict[which(dict$variableName == toupper(trimws(tempSplit[1]))), 'Labels'] <- tempSplit[2]
    i <- i+1
  }
  # --- End getting variable labels

  # Get labelValues
  j <- i
  while(!grepl("(value label|val lab)", controlFile[j], ignore.case = TRUE)) { j <- j+1}
  # first variable exception: does not start with "/"
  # Note: There can be multiple variable names on one line separated by space
  # Note: Exception in SPSS syntax: W_FSTR1 to W_FSTR80
  # Note: In syntax file of 2009 and earlier, need to skip VALUE LABELS
  #j <- j + 1
  varnamelist <- c()
  tempValues <- ""
  while (j <= length(controlFile)) {
    # This if finds the first line that define value label for each variable
    # In some syntax files, it's defined by explicitly saying "value label"
    # In some other files, it's defined with a leading "/"
    if (grepl("^/",controlFile[j]) || grepl("value label|val lab", controlFile[j], ignore.case = TRUE) || grepl("/$|\\.$", controlFile[j-1])) {
      # Assign tempValues to varnamelist value labels
      if (length(varnamelist) != 0) {
        # Cleaning tempValues to return labelValues
        tempValues <- gsub("(^\\^|\\^\\.$|\\.\\^$|\\^\\^$)","",tempValues)
        tempValues <- gsub("\\[[:punct:]]$","",tempValues)
        dict[which(dict$variableName %in% varnamelist), 'labelValues'] <- tempValues
      }
      tempValues <- ""
      varnamelist <- c()

      if (grepl("exe.*\\.$|^miss|^format|^\\.", controlFile[j], ignore.case = T) || controlFile[j] == "") {
        while(!grepl("(value label|val lab)", controlFile[j], ignore.case = TRUE) && j <= length(controlFile)) { j <- j+1}
        next
      }
      # in PISA 2003, variable name can appear on the same line as value labels
      testLine = strsplit(gsub("^/","",controlFile[j])," |\t")[[1]]
      varnamelist <- intersect(toupper(testLine), c(dict$variableName,"TO"))
    }

    if (grepl("\"", controlFile[j])) {
      tempLine <- unlist(strsplit(controlFile[j]," |\t"))
      tempLine <- tempLine[tempLine != ""]
      splitIndex <- grep("\"",tempLine)[1] - 1
      if (!is.na(splitIndex) && splitIndex >= 1) {
        if (!toupper(tempLine[splitIndex]) %in% dict$variableName) {
          splitIndex = splitIndex - 1
        }
        if (splitIndex >= 1) {
          varnamelist <- c(varnamelist, intersect(toupper(tempLine[1:splitIndex]), c(dict$variableName,"TO")))
        }
      }
      tempValuesC <- paste0(tempLine[(splitIndex+1):length(tempLine)], collapse = " ")
      tempValuesC <- gsub("/$|\\.$","", tempValuesC)
      if (grepl("^\"|^\'",tempValuesC)) { # keys are characters
        tempValuesC <- unlist(strsplit(tempValuesC, "\""))
        tempValuesC <- tempValuesC[!tempValuesC %in%  c(""," ")]
        tempValuesC <- gsub("^\'|\'$","", tempValuesC)
        tempValuesC <- paste(tempValuesC[seq(1,length(tempValuesC),2)],
                             gsub("=","",tempValuesC[seq(2,length(tempValuesC),2)]),sep = "=", collapse = "^")

      } else { #keys are numeric
        tempValuesC <- paste(gsub("\"","",gsub(" \"","=",gsub("=","",unlist(strsplit(tempValuesC, "\" "))))),
                             collapse = "^")
      }
      tempValuesC <- trimws(tempValuesC)
      tempValues <- gsub("^\\^","",paste(tempValues, tempValuesC, sep = "^"))

    } else {
      varnamelist <- c(varnamelist, intersect(toupper(unlist(strsplit(controlFile[j]," |\t"))), c(dict$variableName,"TO")))
    }

    varnamelist <- toupper(varnamelist[varnamelist != ""])
    # some spss syntax include "to" to indicate variable names in sequence
    # i.e. wt1 to wt3 to represent wt1, wt2, wt3
    index_of_to <- which(tolower(varnamelist) == "to") # might be more than 1
    if (length(index_of_to) != 0) {
      additional <- c()
      for (ii in index_of_to) {
        index_of_first <- which(dict$variableName == toupper(varnamelist[ii-1]))
        index_of_last <- which(dict$variableName == toupper(varnamelist[ii+1]))
        additional <- c(additional, dict$variableName[seq(index_of_first,index_of_last,1)])
      }
      varnamelist <- c(varnamelist, additional)
      varnamelist <- varnamelist[tolower(varnamelist) != "to"]
    }
    j <- j + 1
  }
  dict$labelValues[is.na(dict$labelValues)] <- ""
  # --- End getting labelValues

  # missing and labels
  dict$labelled <- logical(nrow(dict))
  dict$missing <- ""
  missing_rules <- c(9,99,999,9999,99999,999999,
                     8,98,998,9998,99998,999998,
                     7,97,997,9997,99997,999997,
                     96,996,9996,99996,999996)
  for (ri in 1:nrow(dict)) {
    lv <- dict$labelValues[ri]
    keysTemp <- strsplit(unlist(strsplit(lv,"^",fixed = TRUE)),
                     "=")
    keys <- sapply(keysTemp, function(k) k[1])
    keys <- keys[keys != ""]
    missing <- intersect(missing_rules, keys)
    dict$labelled[ri] <- length(missing) < length(keys)
    if(length(missing) != 0) {
      dict$missing[ri] <- paste0(missing,collapse = ";")
    }
  }
  # --- End getting missing values and labels

  # Get Type (which PV)
  dict$Type <- sapply(dict$variableName, function(zzz){
    if(grepl("pv[1-9]", zzz, ignore.case = TRUE)){
      return(ifelse(substring(zzz,4,4) == 0,substring(zzz,5),substring(zzz,4))) # for PV10
    } else  {
      return ("")
    }
  })

  # Get pvWt and weights
  dict$pvWt <- ""
  # This if finds the first line that define value label for each variable
  # In some syntax files, it's defined by explicitly saying "value label"
  # In some other files, it's defined with a leading "/"
  dict$pvWt <- mapply(function(v,t) {
    if (!grepl("^pv",v, ignore.case = T)) {
      return("")
    } else {
      gsub(paste("pv",t,sep = "|"),"",v, ignore.case = T)
    }
  }, dict$variableName, dict$Type)
  dict$pvWt[grepl("W_.*[0-9]$", dict$variableName, ignore.case = T)] <- gsub("[^0-9]","",dict$variableName[grepl("W_.*[0-9]$", dict$variableName,ignore.case = T)])
  dict$pvWt[is.na(dict$pvWt)] <- ""
  dict$weights <- grepl("^w_.*[^0-9]$", dict$variableName, ignore.case = TRUE)

  # For PISA 2000, weights for reading, math, and science are re-adjusted
  if(grepl("2000_SPSS_student_math", filename)) {
    dict$variableName <- gsub("W_FSTR","W_FSTR_MATH",dict$variableName)
    dict$variableName <- gsub("W_FSTUWT","W_FSTUWT_MATH", dict$variableName)
  }
  if(grepl("2000_SPSS_student_read", filename)) {
    dict$variableName <- gsub("W_FSTR","W_FSTR_READ",dict$variableName)
    dict$variableName <- gsub("W_FSTUWT","W_FSTUWT_READ", dict$variableName)
  }
  if(grepl("2000_SPSS_student_scie", filename)) {
    dict$variableName <- gsub("W_FSTR","W_FSTR_SCIE",dict$variableName)
    dict$variableName <- gsub("W_FSTUWT","W_FSTUWT_SCIE", dict$variableName)
  }

  # Return a data.frame that stores data variable information (width, start, end, labels, etc.)
  dict$variableName <- trimws(dict$variableName, which = "right")

  # create meta files
  cacheFile <- list(datFile = datFname,
                    dict = dict,
                    ver = packageVersion("EdSurvey"),
                    cacheFileVer=4,
                    ts = Sys.time())
  saveRDS(cacheFile, gsub("\\.txt$","\\.meta",filename))
  return(cacheFile)
}



# Used to merge fileFormat data frames
# @return a list that includes:
# 1. FFname = filename of merged file format data.frame
# 2. fileFormat = merged file format data.frame
#' @author Trang Nguyen
#' @importFrom utils read.csv
mergeFF <- function(filepath, LafDictList, by, mergeSuffixes) {
  mainLabelsFile <- LafDictList[[1]]$dict
  mainFileName <- gsub("\\.txt","",LafDictList[[1]]$datFile)
  if (length(LafDictList) < 2) {
    warning("There is only one dataset. No need to merge")
    return(list(fileFormat = mainLabelsFile))
  }
  oldnames <- mainLabelsFile$variableName
  by <- toupper(by)
  if (length(by) != length(LafDictList) - 1) {
    stop(paste0("length of by list is compatible with length of merge files."))
  }
  # If by is a list of variables
  for (i in 2:length(LafDictList)) {
    newLabelsFile <- LafDictList[[i]]$dict
    newnames <- newLabelsFile$variableName
    index_of_by <- which(newnames %in% by[i-1])
    junkvars <- setdiff(intersect(oldnames,newnames),by[i-1])
    index_of_junk <- which(newnames %in% junkvars)

    # Rewrite labelsfile
    index_of_removed <- c(index_of_by, index_of_junk)
    newLabelsFile <- newLabelsFile[-index_of_removed,]
    newLabelsFile$Start[1] <- mainLabelsFile$End[nrow(mainLabelsFile)] + 1
    for (r in 2:nrow(newLabelsFile)) {
      newLabelsFile$Start[r] <- newLabelsFile$Start[r-1] + newLabelsFile$Width[r-1]
    }
    newLabelsFile$End <- newLabelsFile$Start + newLabelsFile$Width - 1
    mainLabelsFile <- rbind(mainLabelsFile, newLabelsFile)

    # Finish up before continue with the loop
    mainLabelsFile <- mainLabelsFile[order(mainLabelsFile$Start),]
    oldnames <- mainLabelsFile$variableName
  }
  # the cache file is the merged file format
  cacheFile <- list(ver = packageVersion("EdSurvey"),
                    cacheFileVer=4,
                    ts = Sys.time(),
                    fileFormat = mainLabelsFile,
                    FFname = paste0("M_FF_", paste0(names(LafDictList),collapse = "_"),"_",mergeSuffixes,".meta"))
  saveRDS(cacheFile, file.path(filepath,paste0("M_FF_", paste0(names(LafDictList),collapse = "_"),"_",mergeSuffixes,".meta")))
  return(cacheFile)
}

# Used fileformat read by readDict to read in SPSS text file
# and then break it down by country to write out csv files
#' @importFrom data.table as.data.table
processMergeTxt <- function(filepath, LafDictList, countries, ff, database, forceReread, verbose) {
  # Checking unprocessed countries
  datbasename <- gsub("FF","DAT", ff$FFname)
  datbasename <- gsub("\\.meta","",datbasename)
  datFnames <- list.files(filepath,
                          pattern = datbasename,
                          full.names = FALSE,
                          ignore.case = TRUE)
  if (file.exists(paste0(filepath,"/",database,"_all-countries.txt")) && !forceReread) {
    countryDict = read.csv(paste0(filepath,"/",database,"_all-countries.txt"), stringsAsFactors = FALSE)
    all_countries <- countryDict$cnt[countryDict$available]
    processDT <- FALSE
  } else {
    # Produce country dictionary
    countryLabels <- unlist(strsplit(ff$fileFormat$labelValues[ff$fileFormat$variableName == "CNT"],"\\^"))
    if (length(countryLabels) == 0) {
      countryLabels <- unlist(strsplit(ff$fileFormat$labelValues[ff$fileFormat$variableName == "COUNTRY"],"\\^"))
      countryDict <- do.call("rbind", strsplit(countryLabels,"="))
      countryDict <- data.frame(countryDict, stringsAsFactors = F)
      colnames(countryDict) <- c("country","country.name")
    } else {
      countryDict <- do.call("rbind", strsplit(countryLabels,"="))
      countryDict <- data.frame(countryDict, stringsAsFactors = F)
      colnames(countryDict) <- c("cnt","country.name")
    }

    # Read Data tables
    # note: used data.table because these files are large.
    # data.table::fread can resolve big memory issue
    masterDTlist <- lapply(LafDictList, function(x) {
      fname = paste0(filepath, "/",x$datFile)
      text <- fread(fname, colClasses = "character", sep = "\n", header = FALSE, strip.white = FALSE, verbose = FALSE, showProgress=FALSE)
      text[,cnt_index := stringi::stri_sub(V1,x$dict$Start[x$dict$variableName == "CNT"],x$dict$End[x$dict$variableName == "CNT"])]
      return(text)
    })
    names(masterDTlist) <- names(LafDictList)

    # PISA 2003 does not have dictionary for alphabetical country code
    if (is.null(countryDict$cnt)) {
      country_index <- unlist(LafDictList[[1]]$dict[LafDictList[[1]]$dict$variableName == "COUNTRY",c("Start","End")])
      temp_country <- masterDTlist[[1]][,{cnt = cnt_index
      country = stringi::stri_sub(V1,country_index[1], country_index[2])
      list(cnt = unique(cnt), country = unique(country))}]
      countryDict <- merge(countryDict, temp_country, by = "country")
    }
    all_countries = unique(masterDTlist[[1]][,cnt_index])
    countryDict$available <- countryDict$cnt %in% all_countries
    write.csv(countryDict, paste0(filepath,"/",database,"_all-countries.txt"), row.names = FALSE)
    processDT <- TRUE
  }

  # get the list of countries to be processed
  if (countries[1] == "*") { countries = all_countries}
  countries = toupper(countries)
  for (c in countries) {
    if (!c %in% all_countries) {
      warning("The database does not have data for the country ",sQuote(c),". These data will be recorded as NA. ")
    }
  }
  countries <- intersect(countries, all_countries)
  if (length(countries) == 0) {
    stop("All countries specified are not available in the database. \n")
  }
  if (length(datFnames) == 0 || forceReread) {
    unprocessed = countries
  } else {
    processed = gsub(paste0(datbasename,"_"),"",datFnames)
    processed = gsub("\\.txt","", processed)
    unprocessed = setdiff(countries,processed)
  }

  if (length(unprocessed) == 0) {
    return(list(datbasename = datbasename, countries = countries))
  }
  # If need to process some countries
  if (!processDT) {
    masterDTlist <- lapply(LafDictList, function(x) {
      fname = paste0(filepath, "/",x$datFile)
      text <- fread(fname, colClasses = "character", sep = "\n", header = FALSE, strip.white = FALSE, verbose = FALSE, showProgress=FALSE)
      text[,cnt_index := stringi::stri_sub(V1,x$dict$Start[x$dict$variableName == "CNT"],x$dict$End[x$dict$variableName == "CNT"])]
      return(text)
    })
    names(masterDTlist) <- names(LafDictList)
  }
  datalist <- lapply(1:length(masterDTlist), function(dti) {
    a = split(masterDTlist[[dti]], by = "cnt_index")
    lapply(a, function(cntry) {
      ret = cntry[,apply(LafDictList[[dti]]$dict[,c("Start","End")],1,function(y) stringi::stri_sub(V1,y[1],y[2]))]
      ret = as.data.table(ret)
      colnames(ret) <- tolower(LafDictList[[dti]]$dict$variableName)
      return(ret)
    })
  })
  names(datalist) = names(masterDTlist)
  masterDTlist <- NULL

  for (cntry in unprocessed) {
    if(verbose) {
      cat("Processing data for country code ")
      cat(dQuote(cntry))
      cat("\n")
    }
    mainDat <- datalist[[1]][[cntry]]
    colnames(mainDat) <- gsub("\\.$","", colnames(mainDat))
    for (i in 2:length(datalist)) {
      newDat <- datalist[[i]][[cntry]]
      if (is.null(newDat)) {
        next # move on to the next available data file to be merged
      }
      mainDat <- merge(mainDat,newDat, by = intersect(c("cnt","schoolid","stidstd"), colnames(newDat)),
                       suffixes = c("",".junk"), all.x = T, all.y = F)
      mainDat <- mainDat[,grep(".junk", colnames(mainDat), invert = TRUE, value = TRUE), with = FALSE]
    }
    missingcolumns <- setdiff(tolower(ff$fileFormat$variableName), colnames(mainDat))
    if (length(missingcolumns)  > 0) {
      mainDat <- mainDat[,(missingcolumns) := NA]
    }
    mainDat <- mainDat[,tolower(ff$fileFormat$variableName), with = FALSE]
    data.table::fwrite(mainDat, file = paste0(filepath,"/",datbasename,"_",cntry,".txt"),
                       sep = ",",col.names = FALSE, na="")
  }
  return(list(datbasename = datbasename, countries = countries))
}

# Used to serialize an existing csv files
catchCountryTxt <- function(filepath, datname, ff) {
  lafFile <- list.files(filepath, pattern=datname, ignore.case=TRUE)
  lafFile <- file.path(filepath, lafFile)
  if (length(lafFile) > 1) {
    mTime <- sapply(lafFile, function(f) { file.info(f)$mtime})
    if (length(which.max(mTime)) == 0) {
      lafFile <- lafFile[1]
    } else {
      lafFile <- lafFile[which.max(mTime)]
    }
  }
  columnTypes = ifelse(ff$fileFormat$dataType=="numeric","double",
                       ifelse(ff$fileFormat$dataType=="character","string",
                              ff$fileFormat$dataType))
  ret <- LaF::laf_open_csv(lafFile, column_types = columnTypes,
               column_names = tolower(ff$fileFormat$variableName))
  return(ret)
}

# Used to look up achievement levels for each year, database, and subject
pisaAchievementHelp <- function(year, database) {
  text <- "year,major,subject,level,lower_limit,database,regex
2000,0,Mathematics,Level 6,669.3,INT,ma
  2000,0,Mathematics,Level 5,606.99,INT,ma
  2000,0,Mathematics,Level 4,544.68,INT,ma
  2000,0,Mathematics,Level 3,482.38,INT,ma
  2000,0,Mathematics,Level 2,420.07,INT,ma
  2000,0,Mathematics,Level 1,357.77,INT,ma
  2000,0,Science,High,NA,INT,scie
  2000,0,Science,Middle,NA,INT,scie
  2000,0,Science,Low,NA,INT,scie
  2000,1,Reading,Level 5,625.61,INT,rea
  2000,1,Reading,Level 4,552.89,INT,rea
  2000,1,Reading,Level 3,480.18,INT,rea
  2000,1,Reading,Level 2,407.47,INT,rea
  2000,1,Reading,Level 1,334.75,INT,rea
  2003,1,Mathematics,Level 6,669.3,INT,ma
  2003,1,Mathematics,Level 5,606.99,INT,ma
  2003,1,Mathematics,Level 4,544.68,INT,ma
  2003,1,Mathematics,Level 3,482.38,INT,ma
  2003,1,Mathematics,Level 2,420.07,INT,ma
  2003,1,Mathematics,Level 1,357.77,INT,ma
  2003,0,Reading,Level 5,625.61,INT,rea
  2003,0,Reading,Level 4,552.89,INT,rea
  2003,0,Reading,Level 3,480.18,INT,rea
  2003,0,Reading,Level 2,407.47,INT,rea
  2003,0,Reading,Level 1,334.75,INT,rea
  2003,0,Science,High,NA,INT,scie
  2003,0,Science,Middle,NA,INT,scie
  2003,0,Science,Low,NA,INT,scie
  2003,0,Problem Solving,NA,NA,INT,pro
  2006,0,Mathematics,Level 6,669.3,INT,ma
  2006,0,Mathematics,Level 5,606.99,INT,ma
  2006,0,Mathematics,Level 4,544.68,INT,ma
  2006,0,Mathematics,Level 3,482.38,INT,ma
  2006,0,Mathematics,Level 2,420.07,INT,ma
  2006,0,Mathematics,Level 1,357.77,INT,ma
  2006,0,Reading,Level 5,625.61,INT,rea
  2006,0,Reading,Level 4,552.89,INT,rea
  2006,0,Reading,Level 3,480.18,INT,rea
  2006,0,Reading,Level 2,407.47,INT,rea
  2006,0,Reading,Level 1,334.75,INT,rea
  2006,1,Science,Level 6,707.94,INT,scie
  2006,1,Science,Level 5,633.33,INT,scie
  2006,1,Science,Level 4,558.73,INT,scie
  2006,1,Science,Level 3,484.14,INT,scie
  2006,1,Science,Level 2,409.54,INT,scie
  2006,1,Science,Level 1,334.94,INT,scie
  2009,0,Science,Level 6,707.94,INT,scie
  2009,0,Science,Level 5,633.33,INT,scie
  2009,0,Science,Level 4,558.73,INT,scie
  2009,0,Science,Level 3,484.14,INT,scie
  2009,0,Science,Level 2,409.54,INT,scie
  2009,0,Science,Level 1,334.94,INT,scie
  2009,1,Reading,Level 6,698.32,INT,rea
  2009,1,Reading,Level 5,625.61,INT,rea
  2009,1,Reading,Level 4,552.89,INT,rea
  2009,1,Reading,Level 3,480.18,INT,rea
  2009,1,Reading,Level 2,407.47,INT,rea
  2009,1,Reading,Level 1a,334.75,INT,rea
  2009,1,Reading,Level 1b,262.04,INT,rea
  2009,1,Reading,Level 5,625.61,ERA,era
  2009,1,Reading,Level 4,552.89,ERA,era
  2009,1,Reading,Level 3,480.18,ERA,era
  2009,1,Reading,Level 2,407.47,ERA,era
  2012,1,Mathematics,Level 6,669.3,INT,ma
  2012,1,Mathematics,Level 5,606.99,INT,ma
  2012,1,Mathematics,Level 4,544.68,INT,ma
  2012,1,Mathematics,Level 3,482.38,INT,ma
  2012,1,Mathematics,Level 2,420.07,INT,ma
  2012,1,Mathematics,Level 1,357.77,INT,ma
  2012,1,Mathematics,Level 6,669.3,CBA,ma
  2012,1,Mathematics,Level 5,606.99,CBA,ma
  2012,1,Mathematics,Level 4,544.68,CBA,ma
  2012,1,Mathematics,Level 3,482.38,CBA,ma
  2012,1,Mathematics,Level 2,420.07,CBA,ma
  2012,1,Mathematics,Level 1,357.77,CBA,ma
  2012,0,Problem Solving,Level 6,683.14,CBA,pro
  2012,0,Problem Solving,Level 5,618.21,CBA,pro
  2012,0,Problem Solving,Level 4,553.28,CBA,pro
  2012,0,Problem Solving,Level 3,488.35,CBA,pro
  2012,0,Problem Solving,Level 2,423.42,CBA,pro
  2012,0,Problem Solving,Level 1,358.49,CBA,pro
  2012,1,Financial Literacy,Level 5,624.63,FIN,lit
  2012,1,Financial Literacy,Level 4,549.86,FIN,lit
  2012,1,Financial Literacy,Level 3,475.1,FIN,lit
  2012,1,Financial Literacy,Level 2,400.33,FIN,lit
  2012,1,Financial Literacy,Level 1,325.57,FIN,lit
  2012,0,Reading,Level 6,698.32,INT,rea
  2012,0,Reading,Level 5,625.61,INT,rea
  2012,0,Reading,Level 4,552.89,INT,rea
  2012,0,Reading,Level 3,480.18,INT,rea
  2012,0,Reading,Level 2,407.47,INT,rea
  2012,0,Reading,Level 1a,334.75,INT,rea
  2012,0,Reading,Level 1b,262.04,INT,rea
  2012,0,Reading,Level 6,698.32,CBA,rea
  2012,0,Reading,Level 5,625.61,CBA,rea
  2012,0,Reading,Level 4,552.89,CBA,rea
  2012,0,Reading,Level 3,480.18,CBA,rea
  2012,0,Reading,Level 2,407.47,CBA,rea
  2012,0,Reading,Level 1a,334.75,CBA,rea
  2012,0,Reading,Level 1b,262.04,CBA,rea
  2012,0,Science,Level 6,707.94,INT,scie
  2012,0,Science,Level 5,633.33,INT,scie
  2012,0,Science,Level 4,558.73,INT,scie
  2012,0,Science,Level 3,484.14,INT,scie
  2012,0,Science,Level 2,409.54,INT,scie
  2012,0,Science,Level 1,334.94,INT,scie
  2015,1,Financial Literacy,Level 5,624.63,FIN,lit
  2015,1,Financial Literacy,Level 4,549.86,FIN,lit
  2015,1,Financial Literacy,Level 3,475.1,FIN,lit
  2015,1,Financial Literacy,Level 2,400.33,FIN,lit
  2015,1,Financial Literacy,Level 1,325.57,FIN,lit
  2015,0,Reading,Level 6,698.32,INT,rea
  2015,0,Reading,Level 5,625.61,INT,rea
  2015,0,Reading,Level 4,552.89,INT,rea
  2015,0,Reading,Level 3,480.18,INT,rea
  2015,0,Reading,Level 2,407.47,INT,rea
  2015,0,Reading,Level 1a,334.75,INT,rea
  2015,0,Reading,Level 1b,262.04,INT,rea
  2015,0,Mathematics,Level 6,669.3,INT,ma
  2015,0,Mathematics,Level 5,606.99,INT,ma
  2015,0,Mathematics,Level 4,544.68,INT,ma
  2015,0,Mathematics,Level 3,482.38,INT,ma
  2015,0,Mathematics,Level 2,420.07,INT,ma
  2015,0,Mathematics,Level 1,357.77,INT,ma
  2015,1,Science,Level 6,707.94,INT,scie
  2015,1,Science,Level 5,633.33,INT,scie
  2015,1,Science,Level 4,558.73,INT,scie
  2015,1,Science,Level 3,484.14,INT,scie
  2015,1,Science,Level 2,409.54,INT,scie
  2015,1,Science,Level 1a,334.94,INT,scie
  2015,1,Science,Level 1b,260.54,INT,scie"
  suppressWarnings(achievementDict <- readr::read_csv(text,na="NA", progress = FALSE))
  achievementDict$level <- paste0("Proficiency ", achievementDict$level)
  temp_subset <- achievementDict[achievementDict$year == year & achievementDict$database == database,]
  ret <- list()
  ret$subjects <- unique(temp_subset$subject)
  ret$regex <- unique(temp_subset$regex)
  ret$default <- temp_subset$subject[temp_subset$major == 1][1]
  ret$achievementLevels <- list()
  for (s in ret$subjects) {
    ret$achievementLevels[[s]] <- temp_subset$lower_limit[temp_subset$subject == s]
    names(ret$achievementLevels[[s]]) <- ifelse(is.na(temp_subset$level[temp_subset$subject == s]),
                                                "Not Defined",
                                                temp_subset$level[temp_subset$subject == s])
  }
  return(ret)
}

# Used to convert short country name to long names
convertCountryName <- function(countryDict, countrycode) {
  return(countryDict$country.name[tolower(countryDict$cnt) == tolower(countrycode)])
}

#' @importFrom utils memory.limit
#' @importFrom haven read_sav write_sav
processPISA2015 <- function(filepath, verbose, countries) {
  memory.limit(max(memory.limit(), 128000)) #COG takes a lot of memory to read in, increase memory limit to allow it to be read in.
  studentSavFileList <- list.files(filepath, pattern = "stu.*\\.sav$", ignore.case=TRUE)
  schoolSavFileList <- list.files(filepath, pattern = "sch.*\\.sav$", ignore.case=TRUE)

  # Read in data
  mainFile <- grep("qqq", studentSavFileList, ignore.case=TRUE)
  if (length(mainFile) == 0) {
    stop("Missing Student Questionaire SPSS (.sav) data file. Since this is the main student file,
         it's required that this data file must be downloaded.")
  }
  full_fnames <- c("stu_qqq","stu_qq2","stu_cog","stu_qtm","stu_flt","stu_cps","sch_qqq")
  savFileList <- sapply(full_fnames, function(f) {
    t <- grep(f,c(studentSavFileList, schoolSavFileList),ignore.case=TRUE, value = TRUE)
    if (length(t) > 1) {
      return(t[1])
    }
    if (length(t) == 0) {
      return(NULL)
    }
    return(t)
  })
  savFileList <- unlist(savFileList)
  if (verbose) {
    cat("Importing SAV data into R \n")
  }
  all_countries <- ""
  studentFFlist <- lapply(savFileList, function(f) {
    if (verbose) {
      cat(paste0("Importing ",sQuote(f),"\n"))
    }
    t <- read_sav(file.path(filepath, f), user_na = TRUE)
    dct <- readDict2015(t)
    CNT2 <- factor(t$CNT)
    ucnt <- levels(CNT2)
    spt <- split(t, CNT2)
    rm(t)
    if(f == savFileList[[1]]) {
      all_countries <<- ucnt
    }
    for(cnti in 1:length(spt)) {
      ci <- names(spt)[cnti]
      f2 <- paste0(gsub(".sav$","",f),"_",ci,".sav")
      if(verbose){
        cat(paste0("  saving tmp ",sQuote(f2),"\n"))
      }
      write_sav(spt[[cnti]], path=file.path(filepath, f2), compress=TRUE)
    }
    rm(spt)
    gc()
    return(dct)
  })

  idlinkage <- list(stu_qqq = "", stu_qq2 = "CNTSTUID", 
                    stu_cog = "CNTSTUID", stu_qtm = "CNTSTUID",
                    stu_flt = "CNTSTUID", stu_cps = "CNTSTUID",
                    sch_qqq = "CNTSCHID")
  idlinkage <- idlinkage[names(idlinkage) %in% names(savFileList)]
  # Merge file formats
  mainLabelsFile <- studentFFlist[[1]]
  if (length(studentFFlist) < 2) {
    warning("There is only one dataset. No need to merge")
    ff <- mainLabelsFile
  } else {
    oldnames <- mainLabelsFile$variableName
    by <- toupper(idlinkage[2:length(idlinkage)])
    if (length(by) != length(studentFFlist) - 1) {
      stop(paste0("length of by list is compatible with length of merge files."))
    }
    # If by is a list of variables
    for (i in 2:length(studentFFlist)) {
      newLabelsFile <- studentFFlist[[i]]
      newnames <- newLabelsFile$variableName
      index_of_by <- which(newnames %in% by[i-1])
      junkvars <- setdiff(intersect(oldnames,newnames),by[i-1])
      # some duplicate vars are actually not junk
      vars_to_exclude_from_junk <- grep('ver_dat|senwt',junkvars,ignore.case=TRUE,value=TRUE)
      junkvars <- junkvars[!junkvars %in% vars_to_exclude_from_junk]
      index_of_junk <- which(newnames %in% junkvars)
      # Rewrite labelsfile
      index_of_removed <- c(index_of_by, index_of_junk)
      newLabelsFile <- newLabelsFile[-index_of_removed,]
      if (length(vars_to_exclude_from_junk) > 0) {
        newLabelsFile$variableName <- ifelse(newLabelsFile$variableName %in% vars_to_exclude_from_junk,
                                             paste0(newLabelsFile$variableName,".",names(studentFFlist)[i]),
                                             newLabelsFile$variableName)
      }
      newLabelsFile$Start[1] <- mainLabelsFile$End[nrow(mainLabelsFile)] + 1
      for (r in 2:nrow(newLabelsFile)) {
        newLabelsFile$Start[r] <- newLabelsFile$Start[r-1] + newLabelsFile$Width[r-1]
      }
      newLabelsFile$End <- newLabelsFile$Start + newLabelsFile$Width - 1
      mainLabelsFile <- rbind(mainLabelsFile, newLabelsFile)
      oldnames <- mainLabelsFile$variableName
    }
    ff <- mainLabelsFile
  }

  ### Merging
  if (countries[1] == "*") {
    countries <- all_countries
  }
  countries <- intersect(toupper(countries), toupper(all_countries))
  for (cntry in countries) {
    if (verbose) {
      cat("Processing data for country code", dQuote(cntry),"\n")
    }
    f2 <- paste0(gsub(".sav$","",savFileList[[1]]),"_",cntry,".sav")
    if(!file.exists(file.path(filepath,f2))) {
      stop("Cache file ", sQuote(file.path(filepath,f2)), " missing.")
    }
    mm <- read_sav(file.path(filepath,f2), user_na=TRUE)
    mm <- UnclassCols(mm)
    colnames(mm) <- gsub("^REPEAT$","REPEATGRADE",colnames(mm))
    colnames(mm) <- gsub("\\.$","",colnames(mm))
    for (li in 2:length(savFileList)) {
      f2 <- paste0(gsub(".sav$","",savFileList[li]),"_",cntry,".sav")
      if(!file.exists(file.path(filepath, f2))) {
        cat(paste(cntry, "does not have", names(savFileList)[li],"data.\n"))
        next
      }
      m2 <- read_sav(file.path(filepath, f2), user_na=TRUE)
      m2 <- UnclassCols(m2)
      colnames(m2) <- gsub("^REPEAT$","REPEATGRADE",colnames(m2))
      colnames(m2) <- gsub("\\.$","",colnames(m2))
      ## Merging data
      mm <- merge(mm, m2, by = idlinkage[[li]], suffixes = c("",".junk"), all.x=TRUE, all.y=FALSE)
      rm(m2)
      var_duplicated_valid <- grep("(senwt|ver_dat)\\.junk",colnames(mm), value=TRUE, ignore.case=TRUE)
      colnames(mm)[which(colnames(mm) %in% var_duplicated_valid)] <- gsub("\\.junk", paste0(".",names(savFileList)[li]),colnames(mm)[which(colnames(mm) %in% var_duplicated_valid)])
      mm <- mm[,grep("\\.junk",colnames(mm), invert=TRUE, value=TRUE, ignore.case=TRUE)]
    }
    mm <- cbind("CNT" = cntry, mm)
    colnames(mm) <- tolower(colnames(mm))
    missingcolumns <- setdiff(tolower(ff$variableName), colnames(mm))
    if (length(missingcolumns)  > 0) {
      naMa <- matrix(NA,nrow=nrow(mm),ncol=length(missingcolumns))
      colnames(naMa) <- missingcolumns
      mm <- cbind(mm,naMa)
    }
    mm <- mm[,tolower(ff$variableName)]
    ## Exporting dat and ff txt file
    for (i in 1:ncol(mm)) {
      if (is.numeric(mm[[i]])) {
        mm[[i]] <- ifelse(is.na(mm[[i]]),"",format(mm[[i]], scientific = FALSE))
      }
    }
    outf <- file.path(filepath,paste0("M_DAT_CY6_MS_CMB_STU_",cntry,".txt"))
    if(file.exists(outf)) {
      unlink(outf)
    }
    data.table::fwrite(mm, file = file.path(filepath,paste0("M_DAT_CY6_MS_CMB_STU_",cntry,".txt")), col.names = FALSE, sep = ",", na="")
    rm(mm)
    gc()
  }

  # Produce country dictionary
  countryLabels <- unlist(strsplit(ff$labelValues[toupper(ff$variableName) == "CNT"],"\\^"))
  countryDict <- do.call("rbind", strsplit(countryLabels,"="))
  countryDict <- data.frame(countryDict, stringsAsFactors = F)
  colnames(countryDict) <- c("cnt","country.name")
  countryDict$available <- (tolower(countryDict$cnt) %in% tolower(all_countries))
  cacheFile <- list(countryDict=countryDict, dict=ff,
                    list_files=names(savFileList),
                    ver=packageVersion("EdSurvey"),
                    cacheFileVer=4, ts=Sys.time())
  saveRDS(cacheFile, file.path(filepath, "INT_all-countries.meta"))
  return(cacheFile)
}

readDict2015 <- function(l) {
  # Build a dataframe that stores information for each variable =========================================================
  # colInfo is a temporary data.frame that stores information on each variable (format, start, end, data type)
  
  l <- UnclassCols(l)
  colnames(l) <- gsub("^REPEAT$","REPEATGRADE",colnames(l))
  colInfo <- data.frame(variableName=colnames(l), stringsAsFactors=FALSE)
  colInfo$format <- sapply(colInfo$variableName, function(z) {
    attributes(l[[z]])$format.spss
  })

  colInfo$Decimal <- as.numeric(ifelse(substr(colInfo$format,1,1) == "F", sapply(strsplit(colInfo$format,"\\."), function(x) { tail(x,1) } ), rep(NA, nrow(colInfo)) ))
  colInfo$Decimal[is.na(colInfo$Decimal) && !(tolower(colInfo$class) %in% "date")] <- 0 #dates are omitted based on SPSS class type so they are characters
  colInfo$multiplier <- as.integer(ifelse(is.na(colInfo$Decimal), 1, 10^colInfo$Decimal))
  colInfo$Width <- gsub("[a-zA-Z]","",sapply(strsplit(colInfo$format,"\\."), function(x) { head(x,1) } ))
  colInfo$Width <- as.numeric(colInfo$Width)
  colInfo$Labels <- sapply(colnames(l), function(z) {attributes(l[[z]])$label})
  colInfo$labelValues <- sapply(colnames(l), function(z) {
    attr <- attributes(l[[z]])$labels
    toupper(paste(attr, names(attr), sep="=", collapse="^"))
  })
  colInfo$format <- NULL
  colInfo$Start <-  c(1,1 + cumsum(colInfo$Width))[1:nrow(colInfo)]
  colInfo$End <- cumsum(colInfo$Width)

  # missing and labels
  colInfo$labelled <- logical(nrow(colInfo))
  colInfo$missing <- ""
  missing_rules <- c(9,99,999,9999,99999,999999,99999999,
                     8,98,998,9998,99998,999998,99999998,
                     7,97,997,9997,99997,999997,99999997,
                     96,996,9996,99996,999996,99999996,
                     95,995,9995,99995,999995,99999995)
  for (ri in 1:nrow(colInfo)) {
    lv <- colInfo$labelValues[ri]
    keysTemp <- strsplit(unlist(strsplit(lv,"^",fixed = TRUE)),
                         "=")
    keys <- sapply(keysTemp, function(k) k[1])
    keys <- keys[keys != ""]
    missing <- intersect(missing_rules, keys)
    colInfo$labelled[ri] <- length(missing) < length(keys)
    if(length(missing) != 0) {
      colInfo$missing[ri] <- paste0(missing,collapse = ";")
    }
  }
  # --- End getting missing values and labels

  # Get Type (which PV)
  colInfo$Type <- sapply(colInfo$variableName, function(zzz){
    if(grepl("pv[1-9]", zzz, ignore.case = TRUE)){
      return(ifelse(substring(zzz,4,4) == 0,substring(zzz,5),substring(zzz,4))) # for PV10
    } else  {
      return ("")
    }
  })

  # Get pvWt and weights
  colInfo$pvWt <- ""
  colInfo$pvWt <- mapply(function(v,t) {
    if (!grepl("^pv",v, ignore.case = T)) {
      return("")
    } else {
      gsub(paste("pv",t,sep = "|"),"",v, ignore.case = T)
    }
  }, colInfo$variableName, colInfo$Type)
  colInfo$pvWt[grepl("W_.*[0-9]$", colInfo$variableName, ignore.case = T)] <- gsub("[^0-9]","",colInfo$variableName[grepl("W_.*[0-9]$", colInfo$variableName,ignore.case = T)])
  colInfo$pvWt[is.na(colInfo$pvWt)] <- ""
  colInfo$weights <- grepl("^w_.*[^0-9]$", colInfo$variableName, ignore.case = TRUE)
  ##characters will have an N/A for their decimal value
  colInfo$dataType <- ifelse(colInfo$Decimal %in% 1:32 | (colInfo$Width > 9 & colInfo$Decimal %in% 0), rep("numeric", nrow(colInfo)),
                        ifelse(colInfo$Decimal %in% 0, rep("integer", nrow(colInfo)),
                               rep("character", nrow(colInfo))))

  # repeat is a reserved words so changed to 'repeat.'
  # colInfo$variableName <- ifelse(tolower(colInfo$variableName) == "repeat","repeatgrade", colInfo$variableName)
  return(colInfo)
}

#builds the NAEP dataList object
buildPISA_dataList <- function(LaF, FF){

  dataList <- list()

  #build the list hierarchical based on the order in which the data levels would be merged in getData
  dataList[["Student"]] <- dataListItem(lafObject = LaF,
                                        fileFormat = FF,
                                        levelLabel = "Student",
                                        forceMerge = TRUE,
                                        parentMergeLevels = NULL,
                                        parentMergeVars = NULL,
                                        mergeVars = NULL,
                                        ignoreVars = NULL,
                                        isDimLevel = TRUE)

  return(dataList)
}
