#' @title Read Data to a Data Frame
#'
#' @description Reads in selected columns to a \code{data.frame} or a
#'              \code{light.edsurvey.data.frame}. On an \code{edsurvey.data.frame},
#'              the data are stored on disk.
#'
#' @param data an \code{edsurvey.data.frame} or
#'             a \code{light.edsurvey.data.frame}
#' @param varnames a character vector of variable names that will be returned.
#'                 When both \code{varnames} and
#'                 a \code{formula} are specified, variables associated with both are
#'                 returned. Set to \code{NULL} by default.
#' @param dropUnusedLevels a logical value. When set to the default value of
#'                         \code{TRUE}, drops unused levels of all factor
#'                         variables.
#' @param omittedLevels a logical value. When set to the default value of
#'                      \code{TRUE}, drops those levels of all factor variables
#'                      that are specified in an \code{edsurvey.data.frame}. Use
#'                      \code{print} on an \code{edsurvey.data.frame} to see
#'                      the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of
#'                          \code{TRUE}, uses the default conditions stored in
#'                           an \code{edsurvey.data.frame} to subset the data. Use
#'                          \code{print} on an \code{edsurvey.data.frame} to
#'                          see the default conditions.
#' @param drop a logical value. When set to the default value of \code{FALSE},
#'             when a single column is returned, it is still represented as a
#'             \code{data.frame} and is not converted to a vector.
#' @param formula a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}}.
#'                When included, \code{getData} returns data associated with
#'                all variables of the \code{formula}. When both \code{varnames} and a
#'                formula are specified, the variables associated with both are
#'                returned. Set to \code{NULL} by default.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}.
#'               Can be set as \code{recode} \code{=} \code{list(var1}
#'               \code{=} \code{list(from} \code{=} \code{c("a","b","c"), to}
#'               \code{=} \code{"d"))}. See Examples.
#' @param includeNaLabel a logical value to indicate if \code{NA} (missing) values are
#'                       returned as literal \code{NA} values or as factor levels
#'                       coded as \code{NA}.
#' @param addAttributes a logical value set to \code{TRUE} to get a
#'                      \code{data.frame} that can be used in calls to
#'                      other functions that usually would take an
#'                      \code{edsurvey.data.frame}. This \code{data.frame} is also called \code{light.edsurvey.data.frame}.
#'                      See Details section in \code{\link{edsurvey.data.frame}} for
#'                      more information on \code{light.edsurvey.data.frame}.
#' @param returnJKreplicates a logical value indicating if JK replicate weights
#'                           should be returned. Defaults to \code{TRUE}.
#'
#' @details By default, an \code{edsurvey.data.frame} does not have data read
#' into memory until \code{getData} is called and returns a data frame.
#' This structure allows \code{EdSurvey} to have a minimal memory footprint.
#' To keep the footprint small, you need to limit \code{varnames} to just
#' the necessary variables.
#'
#' When \code{getData} is called, it returns a \code{data.frame}. When the
#' \code{addAttributes} argument is set to \code{TRUE}, that \code{data.frame}
#' has several attributes added to make it usable by the functions in 
#' the \code{EdSurvey} package (e.g., \code{lm.sdf}), and the class is a
#' \code{light.edsurvey.data.frame}.
#'
#' Note that if both \code{formula} and \code{varnames} are populated, the
#' variables on both will be included.
#'
#' See the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-getData.pdf}{getData}
#' for long-form documentation on this function.
#'
#' @return When \code{addAttributes} is \code{FALSE}, returns a
#' \code{data.frame} containing data associated with requested
#' variables. When \code{addAttributes} is \code{TRUE}, returns a
#' \code{light.edsurvey.data.frame}.
#'
#' @seealso \code{\link{subset.edsurvey.data.frame}} for how to remove
#'          rows from the output
#' @author Tom Fink, Paul Bailey, and Ahmad Emad
#' @example man\examples\getData.R
#' @importFrom LaF laf_open_fwf laf_open_fwf
#' @export
getData <- function(data,
                    varnames=NULL,
                    drop= FALSE,
                    dropUnusedLevels=TRUE,
                    omittedLevels=TRUE, 
                    defaultConditions=TRUE,
                    formula = NULL,
                    recode = NULL,
                    includeNaLabel=FALSE,
                    addAttributes=FALSE,
                    returnJKreplicates=TRUE) {
  # check inputs
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  sdf <- data
  data <- NULL
  if(!inherits(varnames, c("NULL", "character"))) stop("The ", sQuote("varnames"), " argument must be either NULL or a character vector.")
  if(!inherits(formula, c("NULL", "formula"))) stop("The ", sQuote("formula"), " argument must be either NULL or a formula.")
  if(!is.logical(drop)) stop("The ", sQuote("drop"), " argument must be logical.")
  if(!is.logical(dropUnusedLevels)) stop("The ", sQuote("dropUnusedLevels"), " argument must be logical.")
  if(!is.logical(omittedLevels) & !is.vector(omittedLevels)) stop("The ", sQuote("omittedLevels"), " argument must be logical or a character vector.")
  if(!is.logical(defaultConditions)) stop("The ", sQuote("defaultConditions"), " argument must be logical.")
  if(!is.null(recode) & !is.list(recode)) stop(paste0("The ", sQuote("recode"), " argument must be a list."))
  if(is.null(varnames) & is.null(formula)) stop(paste0("At least one of ", sQuote("varnames"), " and ", sQuote("formula"), "must be not NULL."))
  
  # for edsurvey.data.frame.list, just return a list of results
  if (inherits(sdf, c("edsurvey.data.frame.list"))) {
    return(itterateESDFL(match.call(),sdf))
  }

  if(!inherits(sdf, "edsurvey.data.frame")) {
    # if this is a light.edsurvey.data.frame
    if(!missing(defaultConditions)) {
      warning(paste0("The argument ",sQuote("defaultConditions"), " is defined but will be ignored because the incoming data set is already a ", sQuote("light.edsurvey.data.frame"), "."))
    }
  }
  
  #LaF objects hold file format info and file connections.
  #Ensure the file connections are open for reading.
  #OS has limits on maximum open file connections so only open when needed, and close when not in use.
  sdf <- openLaFConnections(sdf)
  
  # get variables from formula
  formulaVars <- all.vars(formula)
  varnames <-c(varnames,formulaVars)
  #Retrieve the default conditions
  dConditions <- NULL
  varNamesDefaults <- c()
  if(defaultConditions & !is.null(sdf$defaultConditions)) {
    if(inherits(sdf, "edsurvey.data.frame")) {
      dConditions <- sdf$defaultConditions[[1]]
    } else {
      dConditions <- attr(sdf, "defaultConditions")[[1]]
    }
    varNamesDefaults <- all.vars(dConditions)
  }
  
  #check if the variable names in recodes are in the data
  varRecodes <- c()
  if(!is.null(recode)) {
    varRecodes = names(recode)
    # create a vector of bad recodes
    badRecodes <- recode[!varRecodes %in% colnames(sdf)]
    if(length(badRecodes)>0) { # if there are elements on the vector
      warning(paste0("Recode variables ", paste(sQuote(badRecodes), collapse=", "), " not found in dataset."))
    }
    sdf <- recode.sdf(sdf, recode)
    recode <- NULL
  }
  
  #build the school/student merge link criteria if applicable
  schoolMergeVarStudent <- NULL
  schoolMergeVarSchool <- NULL
  #Test to ensure all merge variables are present to make the merge
  if(inherits(sdf, "edsurvey.data.frame") && !is.null(sdf$dataListMeta$student$school)) {
    schoolMergeVarStudent <- unlist(strsplit(sdf$dataListMeta$student$school, ";", fixed = TRUE)) #first split on ';' as major fields
    schoolMergeVarStudent <- sapply(schoolMergeVarStudent, function(x){
      head(unlist(strsplit(x, "^", fixed = TRUE)), n=1) #get first item from split in case we need to merge on variables with different names (var1^var2)
    }, simplify = TRUE)
    if(!is.null(schoolMergeVarStudent)) {
      if(sum(!schoolMergeVarStudent %in% names(sdf$data))>0) {
        sdf <- closeLaFConnections(sdf)
        stop(paste0("Merge variable(s) ", pasteItems(sQuote(schoolMergeVarStudent)), " not found in dataset."))
      }
    }
    
    schoolMergeVarSchool <- unlist(strsplit(sdf$dataListMeta$student$school, ";", fixed = TRUE))
    schoolMergeVarSchool <- sapply(schoolMergeVarSchool, function(x){
      tail(unlist(strsplit(x, "^", fixed = TRUE)), n=1) #get last item from split
    }, simplify = TRUE)
    if(!is.null(schoolMergeVarSchool)) {
      if(sum(!schoolMergeVarSchool %in% names(sdf$dataSch))>0) {
        sdf <- closeLaFConnections(sdf)
        stop(paste0("Merge variable(s) ", pasteItems(sQuote(schoolMergeVarSchool)), " not found in dataset."))
      }
    }
  } # end if(inherits(sdf, "edsurvey.data.frame") && !is.null(sdf$dataListMeta$student$school))
  
  #build the teacher/student merge link critiera if applicable
  teacherMergeVarStudent <- NULL
  teacherMergeVarTeacher <- NULL
  #Test to ensure all merge variables are present to make the merge
  if(inherits(sdf, "edsurvey.data.frame") && !is.null(sdf$dataListMeta$student$teacher)) {
    teacherMergeVarStudent <- unlist(strsplit(sdf$dataListMeta$student$teacher, ";", fixed = TRUE))
    teacherMergeVarStudent <- sapply(teacherMergeVarStudent, function(x){
      head(unlist(strsplit(x, "^", fixed = TRUE)), n=1) #get first item from split
    }, simplify = TRUE)
    if(!is.null(teacherMergeVarStudent)) {
      if(sum(!teacherMergeVarStudent %in% names(sdf$data))>0) {
        sdf <- closeLaFConnections(sdf)
        stop(paste0("Merge variable(s) ", pasteItems(sQuote(teacherMergeVarStudent)), " not found in dataset."))
      }
    }
    
    teacherMergeVarTeacher <- unlist(strsplit(sdf$dataListMeta$student$teacher, ";", fixed = TRUE))
    teacherMergeVarTeacher <- sapply(teacherMergeVarTeacher, function(x){
      tail(unlist(strsplit(x, "^", fixed = TRUE)), n=1) # get last item from split
    }, simplify = TRUE)
    if(!is.null(teacherMergeVarTeacher)) {
      if(sum(!teacherMergeVarTeacher %in% names(sdf$dataTch))>0) {
        sdf <- closeLaFConnections(sdf)
        stop(paste0("Merge variable(s) ", pasteItems(sQuote(teacherMergeVarTeacher)), " not found in dataset."))
      }
    }
  } # end if(inherits(sdf, "edsurvey.data.frame") && !is.null(sdf$dataListMeta$student$teacher))
  
  #Retrieve user conditions
  userConditions <- getAttributes(sdf, "userConditions")
  varNamesConditions <- c()
  if(length(userConditions)>0) {
    for (i in c(1:length(userConditions))) {
      if (!is.null(names(userConditions)[i]) && names(userConditions)[i] %in% "recode") {
        varNamesConditions <- c(varNamesConditions, unique(names(userConditions[[i]]))) 
      } else {
        condition <- userConditions[[i]]
        varNamesConditions <- c(varNamesConditions, all.vars(condition))
      }
    }
  }
  if(!inherits(sdf, "edsurvey.data.frame")) {
     varNamesConditions <- c()
  }
  
  #Using the conditions and default variables and varnames to read in data from LaF
  varNamesDefaults2 <- varNamesDefaults
  varNamesDefaults <- varNamesDefaults[which(varNamesDefaults %in% colnames(sdf))]
  varnamesAllConditions <- c(varNamesConditions, varNamesDefaults)
  vars <- c()
  v <-c ()
  
  hpv <- hasPlausibleValue(varnames, sdf) # boolean vector that is TRUE when the variable has plausible values
  iw <- isWeight(varnames, sdf) # boolean vector that is TRUE when the variable is a weight
  vars <- c(vars, varnames[!(iw | hpv)])
  vars_exclude_omitted <- c()
  if(sum(hpv)>0) {
    pvs <- getPlausibleValue(varnames[hpv], sdf)
    vars_exclude_omitted <- c(vars_exclude_omitted, pvs[-1])
    vars <- c(vars, pvs)
  }
  if(sum(iw)>0 & returnJKreplicates == TRUE) {
    invisible(sapply(varnames[iw], function(x) {
      v <- getWeightJkReplicates(x, sdf)
      vars_exclude_omitted <<- c(vars_exclude_omitted, v[-1])
      vars <<- c(vars, v, x)
    }))
  }
  if (sum(iw)>0 & returnJKreplicates == FALSE) {
    vars <- c(vars, varnames[iw])
  }
  # remove any duplicates (e.g. because part of default conditions and requested)
  varnames <- unique(vars)
  
  # expand varnamesAllConditions when it has a variable with PVs or is a weight
  # also make varnamesTotal
  vars <- c()
  v <- c()
  if(inherits(sdf, "edsurvey.data.frame")) {
    for(i in (1:length(varnamesAllConditions))) {
      if(length(varnamesAllConditions) == 0){
        break
      }
      if(hasPlausibleValue(varnamesAllConditions[i], sdf)) {
        v <- getPlausibleValue(varnamesAllConditions[i], sdf)
        vars <- c(vars, v)
      } else {
        if(isWeight(varnamesAllConditions[i], sdf)) {
          v <- getWeightJkReplicates(varnamesAllConditions[i], sdf)
          vars <- c(vars, varnamesAllConditions[i],v)
        } else{
          vars <- c(vars, varnamesAllConditions[i])
        }
      }
    } # end for(i in (1:length(varnamesAllConditions)))
    varnamesAllConditions <- vars
    varnamesTotal <- c(varnamesAllConditions, varnames, varRecodes)
  } else { # end if(inherits(sdf, "edsurvey.data.frame"))
    varnamesTotal <- c(varnames)
  }
  varnamesTotal <- unique(varnamesTotal)
  vars_exclude_omitted <- c(vars_exclude_omitted, varnamesAllConditions[!varnamesAllConditions %in% varnames]) # exclude variables in userConditions but not in called variables
  
  # this section is for retreiving the data from a LaF
  if(inherits(sdf, "edsurvey.data.frame")) {
    #Retrieve the LAF objects from the data list to work with
    dataLaf <- sdf$data
    dataSchLaf <- sdf$dataSch
    dataTchLaf <- sdf$dataTch
    
    ndsl1 <- names(dataSchLaf)
    hasSchoolVars <- ndsl1[!(ndsl1 %in% c(schoolMergeVarSchool, names(dataLaf)))] #remove any merge vars or vars that overlap with the student file from our named list::char
    hasSchoolVars <- hasSchoolVars %in% varnamesTotal[!(varnamesTotal %in% schoolMergeVarSchool)] #check if any school vars exist that are needed that are not merge vars::logical
    hasSchoolVars <- any(hasSchoolVars) #collapse the previous test into one true/false logical
    
    if (!hasSchoolVars){ #set the merge vars to null if not needed
      schoolMergeVarSchool <- NULL
      schoolMergeVarStudent <- NULL
    }
    #grab the index of the var names required in case needed later (include mergeVars)
    indexSchVars <- ndsl1 %in% c(varnamesTotal, schoolMergeVarSchool)
   
    ndsl2 <- names(dataTchLaf)
    hasTeacherVars <- ndsl2[!(ndsl2 %in% c(teacherMergeVarTeacher, names(dataLaf)))] #remove any merge vars or vars that overlap with the teacher file from our named list::char
    hasTeacherVars <- hasTeacherVars %in% varnamesTotal[!(varnamesTotal %in% teacherMergeVarTeacher)] #check if any teacher vars exist that are needed that are not merge vars::logical
    hasTeacherVars <- any(hasTeacherVars)
    
    if (!hasTeacherVars){ #set the merge vars to null if not needed
      teacherMergeVarTeacher <- NULL
      teacherMergeVarStudent <- NULL
    }
    #grab the index of the var names required in case needed later (include mergeVars)
    indexTchVars <- ndsl2 %in% c(varnamesTotal, teacherMergeVarTeacher) #logical empty if no teacherLaf

    missingVars <- varnames[!varnames %in% vars_exclude_omitted & !varnames %in% colnames(sdf)] #ensure we don't have any missing vars::be sure to exclude any vars 'vars_exclude_omit'
    if(length(missingVars) >0 ) {
      sdf <- closeLaFConnections(sdf)
      stop(paste0("The following variable names are required for this call and are not on the incoming data ", paste(sQuote(missingVars), collapse=", "),"."))
    }
    
    #get the numeric indexes for the student data we want
    indexVars <- (1:length(names(dataLaf)))[names(dataLaf) %in% c(varnamesTotal, schoolMergeVarStudent, teacherMergeVarStudent)]
    
    # get the student data using the indexes
    data <- dataLaf[,indexVars, drop=FALSE]
    
    #perform the actual student->school merge if applicable
    if(!is.null(dataSchLaf) && hasSchoolVars==TRUE) { #grab and merge the school level data if necessary
      if(any(indexSchVars) && hasSchoolVars==TRUE) {#test if we have any school vars
        indexSchVars <- (1:length(ndsl1))[indexSchVars] #convert boolean vector to numeric index vector of TRUE values
        
        # get school level data
        datas <- dataSchLaf[,indexSchVars, drop=FALSE]
        
        data$oorder__zz11qq <- 1:nrow(data) # retain original order
        data <- merge(data, datas, by.x=schoolMergeVarStudent, by.y=schoolMergeVarSchool, all.x=TRUE, all.y=FALSE, suffixes = c("", ".dupe"))
        data <- data[,names(data)[!grepl("\\.dupe$",names(data))]] #remove any duplicate fields::student file will take precedent
        
        #test if any schoolMergeVarSchool variables are in the vars to export as these are not included if differently named between datasets, also ensure this variable is requested for output
        if(length(schoolMergeVarSchool[!(schoolMergeVarSchool %in% schoolMergeVarStudent)])>0 && any(schoolMergeVarSchool[!(schoolMergeVarSchool %in% schoolMergeVarStudent)] %in% varnamesTotal)){
          varsToAdd <- schoolMergeVarSchool[!(schoolMergeVarSchool %in% schoolMergeVarStudent)]
          varsToAddIndx <- varsToAdd %in% schoolMergeVarSchool #creates logical vector
          varsToAddIndx <- (1:length(schoolMergeVarSchool))[varsToAddIndx] #converts logical vector into numeric index
          appendData <- data.frame(data[, schoolMergeVarStudent[varsToAddIndx]])
          names(appendData) <- schoolMergeVarSchool[varsToAddIndx]
          varnamesTotal <- c(varnamesTotal, names(appendData)) #be sure to add this back into the varnames total
          data <- cbind(data, appendData) #duplicate the fields if need by since we are dealing with multiple names of the same field
        }
        
        data <- data[order(data$oorder__zz11qq),]
        data$oorder__zz11qq <- NULL
        
      } #end if(sum(nchar(dataSchLaf[indexSchVars]))>1 && hasSchoolVars==TRUE)
    } #end if(!is.null(dataSchLaf) && hasSchoolVars==TRUE)
    
    #perform the actual student->teacher OR (student+schoool)->teacher merge if applicable
    if(!is.null(dataTchLaf) && hasTeacherVars==TRUE) { #grab and merge the teacher level data if necessary
      if(any(indexTchVars) && hasTeacherVars==TRUE) {
        indexTchVars <- (1:length(ndsl2))[indexTchVars] #convert boolean vector to numeric index vector of TRUE values
        
        # get teacher level data
        datas <- dataTchLaf[,indexTchVars, drop=FALSE]
        data$oorder__zz11qq <- 1:nrow(data) # retain original order
        data <- merge(data, datas, by.x=teacherMergeVarStudent, by.y=teacherMergeVarTeacher, all.x=TRUE, all.y=FALSE, suffixes = c("", ".dupe"))
        data <- data[,names(data)[!grepl("\\.dupe$",names(data))]] #remove any duplicate fields::student file will take precedent
        
        #test if any teacherMergeVarTeacher variables are in the vars to export as these are not included if differently named between datasets
        if(length(teacherMergeVarTeacher[!(teacherMergeVarTeacher %in% teacherMergeVarStudent)])>0 && any(teacherMergeVarTeacher[!(teacherMergeVarTeacher %in% teacherMergeVarStudent)] %in% varnamesTotal)){
          varsToAdd <- teacherMergeVarTeacher[!(teacherMergeVarTeacher %in% teacherMergeVarStudent)]
          varsToAddIndx <- varsToAdd %in% teacherMergeVarTeacher #creates logical vector
          varsToAddIndx <- (1:length(teacherMergeVarTeacher))[varsToAddIndx] #converts logical vector into numeric index
          appendData <- data.frame(data[, teacherMergeVarStudent[varsToAddIndx]])
          names(appendData) <- teacherMergeVarTeacher[varsToAddIndx]
          varnamesTotal <- c(varnamesTotal, names(appendData)) #be sure to add this back into the varnames total
          data <- cbind(data, appendData) #duplicate the fields if need by since we are dealing with multiple names of the same field
        }
        
        data <- data[order(data$oorder__zz11qq),]
        data$oorder__zz11qq <- NULL
        
      }#end if(sum(nchar(dataTchLaf[indexTchVars]))>1 && hasTeacherVars==TRUE)
    } #end if(!is.null(dataTchLaf) && hasTeacherVars==TRUE)
     
    
    mergeVarsToRemove <- c(schoolMergeVarSchool, schoolMergeVarStudent, teacherMergeVarStudent, teacherMergeVarTeacher)
    mergeVarsToRemove <- mergeVarsToRemove[!mergeVarsToRemove %in% varnamesTotal] 
    data <- data[,!(names(data) %in% mergeVarsToRemove), drop=FALSE] #remove the unneeded merge vars from the dataset
    varnamesTotal <- names(data) #reset the varnamesTotal variable in case it's out of sync after the merge
    
    #validation check to ensure we have all the fields that were requested
    errorVars <- varnames[!(varnames %in% varnamesTotal)]
    if (sum(nchar(errorVars))>0) {
      sdf <- closeLaFConnections(sdf)
      stop(paste0("The following variable names are required for this call but not found in the data: ", paste(sQuote(errorVars), collapse=", "),"."))
    }
    
    #Apply labels and decimal values:: only build the labels file from unique items to not overlap/combine decimals or other values
    labelsFile1 <- sdf$fileFormat
    
    if(hasSchoolVars){ #only add the labels if we need them
      labelsFile2 <- sdf$fileFormatSchool[!(sdf$fileFormatSchool$variableName %in% labelsFile1$variableName), ]
    } else {
      labelsFile2 <- NULL
    }
    
    if (hasTeacherVars){ #only add the labels if we need them
      labelsFile3 <- sdf$fileFormatTeacher[!(sdf$fileFormatTeacher$variableName %in% c(labelsFile1$variableName, labelsFile2$variableName)), ]
    } else {
      labelsFile3 <- NULL
    }
    
    labelsFile <- rbind(labelsFile1, labelsFile2, labelsFile3) #if null these will not add any additional rows
    
    labelsFile <- labelsFile[order(labelsFile$Start),]
    decimals <- as.numeric(labelsFile$Decimal)
    variables <- labelsFile$variableName
    labels  = list()
    for (i in c(1:dim(labelsFile)[1])) {
      keysTemp = c()
      keys = c()
      values = c()
      variable <- variables[i]
      keysTemp = c(keysTemp , strsplit(labelsFile$labelValues[i],'^', fixed = TRUE)[[1]])
      if(length(keysTemp!=0)) {
        for (j in c(1:length(keysTemp))) {
          keys = c(keys,strsplit(keysTemp[j],'=', fixed = TRUE)[[1]][1])
          
          #in case the label has a true '=' symbol, we will then need to re-join all but the first element
          temp <- paste0(strsplit(keysTemp[j],'=', fixed = TRUE)[[1]][-1], collapse = "=")
          if(temp == "" | is.na(temp)) {
            temp <- "label unknown"
          }
          values = c(values,temp)
          }
          
        labels[[variable]] <- list(keys = keys, values = values)
      }    
    }
    
    #apply the decimal conversion prior to applying the labels AND the merge in case we have a decimal value used as the key in the key/value label or need to adjust any weights
    #PISA reads in csv files so no need to convert to decimal values
    if (!sdf$survey %in% c("PISA","TALIS","PIAAC")) {
      for(i in c(1:length(colnames(sdf)))) {
        varn <- variables[i] # the variable being considered
        decLen <- decimals[i]
        decLen[is.na(decLen)] <- 0 #use a 0 if NA is specified for decimal length (character value)
        if(decLen!=0 & varn %in% varnamesTotal) {
          # if it has a decimal conversion and is in the requested data
          data[,varn] <- data[,varn]/(10^decLen)
        }
      }
    }
    
    #Give warning to user that 'totwgt' (student level weight) may give incorrect results when teacher data is merged
    if (sdf$survey %in% c("TIMSS", "PIRLS", "TIMSS Advanced", "CivED") && hasTeacherVars && any(varnamesTotal %in% c("totwgt"))) {
      warning("Teacher data has been merged.  The student-level 'totwgt' weight variable may not produce correct results in analysis.  See documentation.")
    } #end  if (sdf$survey %in% c("TIMSS", "PIRLS", "TIMSS Advanced") && hasTeacherVars)
    
    #applying labels to the data file
    labelsn <- names(labels)
    for(i in 1:length(labels)) {
      vari <- labelsn[i]
      if(vari %in% names(data)) {
        if(length(unique(labels[[i]]$keys)) != length(labels[[i]]$keys)) {
          warning(paste0("Duplicate variable label key ", dQuote(i), " in variable ",vari,"."))
        }
        # in TIMSS files there are some variables that are real / integer and there is one omitted/invalid code
        # in PIAAC, there is variable that has Missing in value labels but it shouldn't be omitted
        if((sdf$survey != "PIAAC" && all(labels[[i]]$values %in% sdf$omittedLevels))) { #generally these are 99999/999997 etc
          if(is.numeric(suppressWarnings(as.numeric(labels[[i]]$keys)))){
            #fixes any rounding issues comparing source data to the key value (e.g., value of 999998.99999999 need to be compared to key value of 999999)
            data[round(data[,vari],8) %in% labels[[i]]$keys,vari] <- NA 
          }else{
            data[data[,vari] %in% labels[[i]]$keys,vari] <- NA
          }
          
        } else if (!is.null(labelsFile$labelled) && !labelsFile$labelled[labelsFile$variableName == vari]) {
          # fileFormat has missing values and labelled columns
          # keep the missing value labels ----> need to think whether to keep it or not
          # data[data[,vari] %in% labels[[i]]$keys,vari] <- labels[[i]]$values[labels[[i]]$keys %in% labels[[i]]$keys]
          # sdf$omittedLevels <- unique(c(sdf$omittedLevels,labels[[i]]$values))
          data[data[,vari] %in% labels[[i]]$keys,vari] <- NA
        } else {# this is a truly labeled file
          if(length(unique(labels[[i]]$values)) != length(labels[[i]]$values)) {
            tab <- table(labels[[i]]$values)
            dnames <- names(tab[tab > 1])
            needNewLabels <- labels[[i]]$values %in% dnames
            if(vari %in% varnames) {
              warning(paste0("Updating labels on ",sQuote(vari), " because there are multiples of the label ", sQuote(dnames), "."))  
            }
            labels[[i]]$values[needNewLabels] <- paste(labels[[i]]$values[needNewLabels],1:(sum(needNewLabels)), sep=":")
          }
          
          lvls <- labels[[i]]$keys
          lbls <- labels[[i]]$values
          if(includeNaLabel) {
             if(sum(is.na(data[,vari])) > 0) {
               lvls <- c(NA, lvls) #add NA first to the list
               lbls <- c("(Missing)", lbls)
             }
          }
          # some id variables has missing values or special case labels (i.e. bookid)
          if(getAttributes(sdf,"survey") == "PISA" && grepl("id",vari,ignore.case = T)) {
            exception = unique(data[,vari][!data[,vari] %in% lvls])
            lvls <- c(lvls,exception)
            lbls <- c(lbls,exception)
          }
          
          #test if there are defined numeric values not in the label
          if(getAttributes(sdf,"validateFactorLabels")==TRUE){
            exception <- unique(data[,vari][!data[,vari] %in% lvls])
            exception <- exception[!is.na(exception) & trimws(exception, which = "both")!=""]
            if(length(exception)>0){
              lvls <- c(lvls,exception)
              lbls <- c(lbls,exception)
            }
          }
          
          
          data[data[,vari]=="" | is.na(data[,vari]),vari] <- NA 
          suppressWarnings(lvlsp <- as.numeric(lvls))
          suppressWarnings(dvi <- as.numeric(data[,vari]))
          suppressWarnings(lblsp <- as.numeric(lbls))
          if(sum(is.na(lvlsp)) - sum(is.na(lvls)) > 0 || sum(is.na(unique(dvi))) - sum(is.na(unique(data[,vari]))) >0 || sum(!is.na(lblsp)) >0) {
            if(anyNA(data[,vari]) && includeNaLabel){
              data[,vari] <- factor(data[,vari], levels=lvls, labels=lbls, exclude = NULL)
            }else{
              data[,vari] <- factor(data[,vari], levels=lvls, labels=lbls) #NA values excluded by default
            }
          } else {
            if(anyNA(data[,vari]) && includeNaLabel){
              data[,vari] <- lfactor(dvi, levels=lvlsp, labels=lbls, exclude = NULL)
            }else{
              data[,vari] <- lfactor(dvi, levels=lvlsp, labels=lbls) #NA values excluded by default
            }
          }
        } # end else for if(length(labels[[i]]$values) == 1 && labels[[i]]$values == "OMITTED OR INVALID" && nchar(gsub("9","",labels[[i]]$keys)) == 0)
      } else { # end if(vari %in% names(data))
        # you get here if there is a variable on the labels file but not in the data.
        # nothing to do
      }# end else for if(vari %in% names(data))
    } # for for(i in 1:length(labels))
    

    # check if variable can be converted to numeric. Some "labels" are just numbers. 
    # this fixes that
    for(var in names(data)) {
      # convToNum converts the variable to a number--if it will not cause data loss
      data[,var] <- convToNum(data[,var])  
    }
    
    #Apply default conditions
    # first check if they are valid
    if (any(!varNamesDefaults2 %in% names(data))) {
      ind <- varNamesDefaults2[which(!varNamesDefaults2 %in% names(data))]
      warning(paste0(sQuote(ind),
                     " from default conditions not found in data. Setting ",
                     sQuote("defaultConditionts"), " to ", dQuote("FALSE"), "."))
      defaultConditions <- FALSE
    }

    # then actually apply defaultConditions
    if(defaultConditions) {
      if(length(dConditions) > 0) {
        r <- eval(dConditions, data)
        data <- data[r,,drop=FALSE]
      }
    }
  
    #Apply user conditions (i.e. subset, recode)
    if(length(userConditions)>0) {
      for (i in c(1:length(userConditions))) {
        if (!is.null(names(userConditions)[i]) && names(userConditions)[i] %in% "recode") {
          recode <- userConditions[[i]]
          if(!is.null(recode)) {
            # apply recodes
            for (i in 1:length(recode)){
              ni <- names(recode)[i]
              from <- recode[[i]]$from
              to <- recode[[i]]$to
              if (length(to) > 1) {
                stop(paste0("More than one 'To' value found in the ", sQuote(ni) ," element of the 'recode' argument."))
              }
              
              badFrom <- c() #levels with incorrect recodes 
              if(inherits(data[,ni], "factor")) {
                newto <- to
                if(to %in% from) { # remove degenerate recode
                  from <- from[!from %in% to]
                }
                labs <- levels(data[,ni]) # used for both lfactors and factors
                if(newto %in% labs) { # this is not a new label 
                  newto <- NULL
                }
                tmp <- as.character(data[,ni])
                if(inherits(data[,ni],"lfactor")) { # it is an lfactor
                  levs <- llevels(data[,ni])
                  # in case of lfactor:
                  # + from can be numeric or character
                  # + to can be numeric or character
                  # To simplify the code, if to is a numeric, we will coerce it to character
                  if (is.numeric(to)) {
                    if (!to %in% levs) {
                      labs <- c(labs, as.character(to)) # since there are no labels provided, we will use character format of levels
                      levs <- c(levs, to)
                    }
                    toNum <- to
                    to <- labs[levs==to]
                  } else {
                    if (!to %in% labs) {
                      labs <- c(labs,to)
                      toNum <- max(levs,na.rm=TRUE) + 1
                      levs <- c(levs, toNum)
                    } else {
                      toNum <- levs[which(to %in% labs)]
                    }
                  }
                  # after the code above, to is always a character label
                  
                  # from can be a vector of mixed numeric and character values
                  # fromNum: numeric values in from
                  # fromChar: character values in from
                  suppressWarnings(fromNum <- as.numeric(from)) # numeric from variables
                  fromChar <- from[is.na(fromNum)]# character from variables
                  # numeric from variables
                  fromNum <- fromNum[!is.na(fromNum)]
                  
                  # changing tmp according to numeric values of from
                  if(length(fromNum)>0) {
                    tmp_numeric <- lfactors:::switchllevels(data[,ni])
                    tmp[tmp_numeric %in% fromNum] <- to
                    if(any(!fromNum %in% levs)) {
                      #add any missing levels to missing list
                      badFrom <- fromNum[!fromNum %in% levs]
                    }
                    labs <- labs[!levs %in% setdiff(fromNum,toNum)]
                    levs <- levs[!levs %in% setdiff(fromNum,toNum)]
                  }
                  # changing tmp according to character values of from
                  if(length(fromChar)>0) {
                    tmp[tmp %in% fromChar] <- to
                    if(any(!fromChar %in% labs)) {
                      badFrom <- c(badFrom, fromChar[!fromChar %in% labs])
                    }
                    levs <- levs[!labs %in% setdiff(fromChar, to)]
                    labs <- labs[!labs %in% setdiff(fromChar, to)]
                  }
                  # Now we need to call lfactors again to make sure levels are mapped correctly to modified character vectors
                  data[,ni] <- lfactor(tmp, levels=levs, labels=labs, exclude = NULL)
                } else { # end if(inherits(x[,ni],"lfactor"))
                  # it is a base r factor so from and to have to be character
                  tmp[tmp %in% from] <- to
                  if(any(!from %in% labs)) {
                    #add any missing levels to missing list
                    badFrom <- c(badFrom,from[!from %in% labs])
                  }
                  if (!to %in% labs) {
                    labs <- c(labs,to)
                  }
                  data[,ni] <- factor(tmp, levels=labs)
                }
              } else { # end if(inherits(x[,ni], "factor"))
                # recode for non factors 
                if(any(!from %in% data[,ni])) {
                  badFrom <- from[!from %in% data[,ni]]
                }
                data[,ni][data[,ni] %in% from] <- to
              } # end else for if(inherits(data[,ni], "factor"))
              if(length(badFrom) > 0) {
                warning(paste0("When recoding, could not find the level(s) ",
                               pasteItems(dQuote(badFrom), final="or"),
                               " in the variable ", dQuote(ni), "."))
              }
            } # for (i in 1:length(recode))
          } # end if(!is.null(recode))
        } else { # other userConditions are specified in subset
          condition <- userConditions[[i]]
          r <- eval(condition, data)
          r <- ifelse(is.na(r),FALSE,r) #remove NA
          data <- data[which(r),,drop=FALSE]  
        }
      }#end for (i in c(1:length(userConditions)))
    } # end if(length(userConditions) > 0)
    
    # apply omittedLevels when TRUE or equal to some levels
    # this code should execute on all of the variables when omittedLevels is TRUE
    flag <- FALSE # set to TRUE when using ommittedLevels in some capacity
    # lev variable is the levels that are being omitted
    if(omittedLevels == TRUE) {
      lev <- unlist(sdf$omittedLevels) # here we know it is an edsurvey.data.frame
      flag <- TRUE
    }
    if(!is.logical(omittedLevels)) {
      lev <- omittedLevels
      flag <- TRUE
    }
    
    #flag variable is calculated above to determine if omitted values should be excluded from results
    if(flag) {
      keep <- rep(0, nrow(data))
      for (i in 1:length(varnamesTotal)) {
        vari <- varnamesTotal[i]
        if(! vari %in% vars_exclude_omitted) {
          # omit data at these levels
          keep <- keep + (data[,vari] %in%  lev)
        }
      }
      if(sum(keep>0) > 0) {
        # only omit if something gets omitted
        data <- data[keep==0,,drop=FALSE]
      }
    }  
    
    
    # call droplevels on data when dropUnusedLevels=TRUE
    if(dropUnusedLevels) {
      for (i in 1:length(varnamesTotal)) {
        if(is.factor(data[,varnamesTotal[i]])) {
            data[,varnamesTotal[i]] <- droplevels(data[,varnamesTotal[i]])
        }
      }
    }
    
  } # end if(inherits(sdf, "edsurvey.data.frame"))
  else {
    missingVars <- varnames[!varnames %in% vars_exclude_omitted & !varnames %in% colnames(sdf)]
    if(length(missingVars) >0 ) {
      sdf <- closeLaFConnections(sdf)
      stop(paste0("The following variable names are required for this call and are not on the incoming data ", pasteItems(dQuote(missingVars)),"."))
    }
    
    varnamesTotal <- varnames
    data <- sdf
    
    # check omittedLevels argument to see if should be applied
    flag <- FALSE
    if(omittedLevels == TRUE) {
      lev <- unlist(attributes(sdf)$omittedLevels) # this is a light.edsurvey.data.frame
      flag <- TRUE
    }
    if(!is.logical(omittedLevels)) {
      lev <- omittedLevels
      flag <- TRUE
    }
    # if it should be applied, apply omittedLevels
    if(flag) {
      keep <- rep(0, nrow(data))
      for (i in 1:length(varnamesTotal)) {
        vari <- varnamesTotal[i]
        if(! vari %in% vars_exclude_omitted) {
          # omit data at these levels
          keep <- keep + (data[,vari] %in%  lev)
        }
      }
      if(sum(keep>0) > 0) {
        # only omit if something gets omitted
        data <- data[keep==0,,drop=FALSE]
      }
    }
    
    # dropUnusedLevels for a light.edsurvey.data.frame
    if(!missing(dropUnusedLevels)) {
      if(dropUnusedLevels) {
        for (i in 1:length(varnamesTotal)) {
          if(is.factor(data[,varnamesTotal[i]])) {
              data[,varnamesTotal[i]] <- droplevels(data[,varnamesTotal[i]])
          }
        }
      }
    }
    data <- data[,varnames, drop=FALSE]
  } # end else for if(inherits(sdf, "edsurvey.data.frame"))
  # now the variable 'data' has a data.frame in it
  
  sdf <- closeLaFConnections(sdf) #ensure we close the LaF connections
  if(addAttributes) {
    # the user requested a light.edsurvey.data.frame
    # these have everything in attributes (as well as the data already being read in)
    if(nrow(data) == 0) {
      warning("The requested dataset has 0 rows.")
    }
    data <- data[,varnames, drop=drop]
    class(sdf) <- "list"
    # get the names of the attributes
    sdfnames <- names(sdf)
    # exclude the "data" attribute
    sdfnames <- sdfnames[sdfnames!="data"]
    # add every other attribute to "data"
    lapply(sdfnames, function(x){
      dat <- get("data")
      attr(dat, x) <- sdf[[x]]
      data <<- dat
    })
    # reset userConditions to remove recode (because its already applied)
    class(data) <- c("light.edsurvey.data.frame", class(data))
    data <- setAttributes(data,"userConditions", userConditions[which(!names(userConditions) %in% "recode")])
    
    return(data)
  } # end if(addAttributes) 
  if(nrow(data) == 0) {
    warning("The requested dataset has 0 rows.")
  }
  data <- data[,varnames, drop=drop]

  return(data)
}

# convToNum converts the variable to a number--if it will not cause data loss
convToNum <- function(x) {
  
  if(!is.character(x)){
    return(x)
  }
  
  x3 <- trimws(x)
  x3[x3 == ""] <- NA
  suppressWarnings(x2 <- as.numeric(x3))
  if(sum(is.na(x2)) == sum(is.na(x3))) {
    # there are NAs/empty strings in the original data
    # in every pace where there are NAs in the numeric version
    # so return the numeric version
    return(x2)
  }
  # there are values other than the empty string that do not convert to numeric
  # so just keep the text
  return(x)
}

#openLaFConnections ensures any closed LaF connections to files are opened for gathering data
openLaFConnections <- function(sdf) {
  
  #establish LaF connections if they are not opened
  if(!is.null(sdf$data)){#ensure we have a LaF object here the student object
    if(sdf$data@file_id < 0){ #test if the file connection is open or not::if not then we will recreate the LaF from the existing LaF model.  Must supply 'column_types' as character vector in this instance
      if(sdf$data@file_type=="fwf"){
        newLaF <- laf_open_fwf(filename = sdf$data@filename, 
                                    column_types = sdf$fileFormat$dataType, 
                                    column_widths = sdf$data@column_widths, 
                                    column_names = sdf$data@column_names, 
                                    dec = sdf$data@options$dec, 
                                    trim = sdf$data@options$trim)
      }else if(sdf$data@file_type=="csv"){
        newLaF <- laf_open_csv(filename = sdf$data@filename,
                                    column_types = sdf$fileFormat$dataType,
                                    column_names = sdf$data@column_names,
                                    sep = sdf$data@options$sep,
                                    dec = sdf$data@options$dec,
                                    trim = sdf$data@options$trim,
                                    skip = sdf$data@options$skip)
      }else{
        stop(paste0("Unexpected LaF object type provided. Expects the LaF object type of ", sQuote("fwf"), " or ", sQuote("csv"), "."))
      }
      
      
      sdf$data <- newLaF
    }
  }
  if(!is.null(sdf$dataSch)){#ensure we have a LaF object here for the school object
    if(sdf$dataSch@file_id < 0){ #test if the file connection is open or not::if not then we will recreate the LaF from the existing LaF model.  Must supply 'column_types' as character vector in this instance
      if(sdf$dataSch@file_type=="fwf"){
        newLaF <- laf_open_fwf(filename = sdf$dataSch@filename, 
                                    column_types = sdf$fileFormatSchool$dataType, 
                                    column_widths = sdf$dataSch@column_widths, 
                                    column_names = sdf$dataSch@column_names, 
                                    dec = sdf$dataSch@options$dec, 
                                    trim = sdf$dataSch@options$trim)
      }else if(sdf$dataSch@file_type=="csv"){
        newLaF <- laf_open_csv(filename = sdf$dataSch@filename,
                                    column_types = sdf$fileFormatSchool$dataType,
                                    column_names = sdf$dataSch@column_names,
                                    sep = sdf$dataSch@options$sep,
                                    dec = sdf$dataSch@options$dec,
                                    trim = sdf$dataSch@options$trim,
                                    skip = sdf$dataSch@options$skip)
      }else{
        stop(paste0("Unexpected LaF object type provided. Expects the LaF object type of ", sQuote("fwf"), " or ", sQuote("csv"), "."))
      }
      
      sdf$dataSch <- newLaF
    }
  }
  if(!is.null(sdf$dataTch)){#ensure we have a LaF object here for the school object
    if(sdf$dataTch@file_id < 0){ #test if the file connection is open or not::if not then we will recreate the LaF from the existing LaF model.  Must supply 'column_types' as character vector in this instance
      if(sdf$dataTch@file_type=="fwf"){
        newLaF <- laf_open_fwf(filename = sdf$dataTch@filename, 
                                    column_types = sdf$fileFormatTeacher$dataType, 
                                    column_widths = sdf$dataTch@column_widths, 
                                    column_names = sdf$dataTch@column_names, 
                                    dec = sdf$dataTch@options$dec, 
                                    trim = sdf$dataTch@options$trim)
      }else if(sdf$dataTch@file_type=="csv"){
        newLaF <- laf_open_csv(filename = sdf$dataTch@filename,
                                    column_types = sdf$fileFormatTeacher$dataType,
                                    column_names = sdf$dataTch@column_names,
                                    sep = sdf$dataTch@options$sep,
                                    dec = sdf$dataTch@options$dec,
                                    trim = sdf$dataTch@options$trim,
                                    skip = sdf$dataTch@options$skip)
      }else{
        stop(paste0("Unexpected LaF object type provided. Expects the LaF object type of ", sQuote("fwf"), " or ", sQuote("csv"), "."))
      }
      
      
      sdf$dataTch <- newLaF
    }
  }
  
  return(sdf)
}

#openLaFConnections ensures any closed LaF connections to files are opened for gathering data
closeLaFConnections <- function(sdf) {
  
  #establish LaF connections if they are not opened
  if(!is.null(sdf$data)){#ensure we have a LaF object here the student object
    if(!(sdf$data@file_id < 0)){ #test if the file connection is open or not::if not then we will recreate the LaF from the existing LaF model.
      LaF::close(sdf$data)
    }
  }
  if(!is.null(sdf$dataSch)){#ensure we have a LaF object here for the school object
    if(!(sdf$dataSch@file_id < 0)){ #test if the file connection is open or not::if not then we will recreate the LaF from the existing LaF model.
      LaF::close(sdf$dataSch)
    }
  }
  if(!is.null(sdf$dataTch)){#ensure we have a LaF object here for the teacher object
    if(!(sdf$dataTch@file_id < 0)){ #test if the file connection is open or not::if not then we will recreate the LaF from the existing LaF model.
      LaF::close(sdf$dataTch)
    }
  }
  
  return(sdf)
}
