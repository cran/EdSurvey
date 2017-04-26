#' @title Gets data from an edsurvey.data.frame.
#'
#' @description Reads in selected columns.
#'
#' @param data an \code{edsurvey.data.frame} or
#'             \code{light.edsurvey.data.frame}.
#' @param varnames a character vector of variable names that will be returned.
#'                 When both \code{varnames} and
#'                 a formula are specified, variables associated with both are
#'                 returned. Set to \code{NULL} by default.
#' @param schoolMergeVarStudent a character variable name from the student file
#'                              used to merge student and school data files.
#'                              Set to \code{NULL} by default.
#' @param schoolMergeVarSchool a character variable name name from the school
#'                             file used to merge student and school data files
#'                             Set to \code{NULL} by default.
#' @param dropUnusedLevels a logical value. When set to the default value of
#'                         \code{TRUE}, drops unused levels of all factor
#'                         variables.
#' @param omittedLevels a logical value. When set to the default value of
#'                      \code{TRUE}, drops those levels of all factor variables
#'                      that are specified in \code{edsurvey.data.frame}. Use
#'                      \code{print} on an \code{edsurvey.data.frame} to see
#'                      the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of
#'                          \code{TRUE}, uses the default conditions stored in
#'                          \code{edsurvey.data.frame} to subset the data. Use
#'                          \code{print} on an \code{edsurvey.data.frame} to
#'                          see the default conditions.
#' @param drop a logical value. When set to the default value of \code{FALSE},
#'             when a single column is returned, it is still represented as a
#'             \code{data.frame} and is not converted to a vector.
#' @param formula a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}}.
#'                When included, \code{getData} returns data associated with
#'                all variables of the formula. When both \code{varnames} and a
#'                formula are specified, the variables associated with both are
#'                returned. Set to \code{NULL} by default.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}.
#'               Can be set as \code{recode} \code{=} \code{list(var1}
#'               \code{=} \code{list(from} \code{=} \code{c("a","b","c"), to}
#'               \code{=} \code{"d"))}. See examples.
#' @param includeNaLabel a logical value, should \code{NA} (missing) values be
#'                       returned as literal \code{NA}s or as factor levels
#'                       coded as \dQuote{NA}.
#' @param addAttributes a logical value. Set to \code{TRUE} to get a
#'                      \code{data.frame} that can be used in calls to
#'                      other functions that usually would take an
#'                      \code{edsurvey.data.frame}.
#' @param returnJKreplicates a logical value indicating if JK replicate weights
#'                           be returned. Defaults to \code{TRUE}.
#' @details By default an \code{edsurvey.data.frame} does not have data read
#' into memory until \code{getData} is called.
#' This allows for a minimal memory footprint.
#' To keep this footprint small, you need to limit \code{varnames} to just
#' necessary variables. All the data is labeled 
#' according to NAEP documentation. 
#' note that if both \code{formula} and \code{varnames} are populated, the
#' variables on both will be included.
#'
#' For details on using this function, see the vignette available by calling
#' \code{vignette("getData",} \code{package} \code{=} \code{"EdSurvey")} 
#' in R.
#'
#' @return When \code{addAttributes} is \code{FALSE}, returns a
#' \code{data.frame} containing data associated with requested
#' variables. When \code{addAttributes} is \code{TRUE}, returns a
#' \code{light.edsurvey.data.frame}.
#'
#' @seealso \code{\link{subset.edsurvey.data.frame}} for how to remove
#'          rows from the output.
#' @author Ahmad Emad and Paul Bailey
#' @example man\examples\getData.R
#' @export
getData <- function(data,
                    varnames=NULL,
                    drop= FALSE,
                    schoolMergeVarStudent=NULL,
                    schoolMergeVarSchool=NULL,
                    dropUnusedLevels=TRUE,
                    omittedLevels=TRUE, 
                    defaultConditions=TRUE,
                    formula = NULL,
                    recode = NULL,
                    includeNaLabel=FALSE,
                    addAttributes=FALSE,
                    returnJKreplicates=TRUE) {
  # check inputs
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  sdf <- data
  data <- NULL
  if(!inherits(varnames, c("NULL", "character"))) stop("The ", sQuote("varnames"), " argument must be either NULL or a character vector.")
  if(!inherits(formula, c("NULL", "formula"))) stop("The ", sQuote("formula"), " argument must be either NULL or a formula.")
  if(!inherits(schoolMergeVarStudent, c("NULL", "character"))) stop("The ", sQuote("schoolMergeVarStudent"), " argument must be either NULL or a character vector.")
  if(!inherits(schoolMergeVarSchool, c("NULL", "character"))) stop("The ", sQuote("schoolMergeVarSchool"), " argument must be either NULL or a character vector.")
  if(!is.logical(drop)) stop("The ", sQuote("drop"), " argument must be logical.")
  if(!is.logical(dropUnusedLevels)) stop("The ", sQuote("dropUnusedLevels"), " argument must be logical.")
  if(!is.logical(omittedLevels) & !is.vector(omittedLevels)) stop("The ", sQuote("omittedLevels"), " argument must be logical or a character vector.")
  if(!is.logical(defaultConditions)) stop("The ", sQuote("defaultConditions"), " argument must be logical.")
  if(!is.null(recode) & !is.list(recode)) stop(paste0("The ", sQuote("recode"), " argument must be a list."))
  if(is.null(varnames) & is.null(formula)) stop(paste0("At least one of the ", sQuote("varnames"), " and ", sQuote("formula"), " arguments must be not NULL."))

  if(!inherits(sdf, "edsurvey.data.frame")) {
    # if this is a light.edsurvey.data.frame
    if(!missing(defaultConditions)) {
      warning(paste0("The argument ",sQuote("defaultConditions"), " is defined but will be ignored because the incoming data set is already a light.edsurvey.data.frame."))
    }
  }
  
  # get variables from formula
  formulaVars <- all.vars(formula)
  varnames = c(varnames,formulaVars)
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
  
  varRecodes <- c()
  if(!is.null(recode)) {
    varRecodes = names(recode)
    # create a vector of bad recodes
    badRecodes <- recode[!varRecodes %in% names(sdf)]
    if(length(badRecodes)>0) { # if there are elements on the vector
      warning(paste0("Recode variables ", paste(sQuote(badRecodes), collapse=", "), " not found in data."))
    }
  }

  ##add school and student merge vars error messages if using an incorrect variable name
  if(inherits(sdf, "edsurvey.data.frame")) {
    schoolMergeVarStudent <- c(schoolMergeVarStudent)
    if(!is.null(schoolMergeVarStudent)) {
      if(sum(!schoolMergeVarStudent %in% names(sdf$data))>0) {
        stop(paste0("Merge variable(s) ", paste(sQuote(schoolMergeVarStudent), collapse=", "), " not found in data."))
      }
    }
    
    schoolMergeVarSchool <- c(schoolMergeVarSchool)
    if(!is.null(schoolMergeVarSchool)) {
      if(sum(!schoolMergeVarSchool %in% names(sdf$dataSch))>0) {
        stop(paste0("Merge variable(s) ", paste(sQuote(schoolMergeVarSchool), collapse=", "), " not found in data."))
      }
    }
  } # end if(inherits(sdf, "edsurvey.data.frame"))

  #Retrieve user conditions
  userConditions <- getAttributes(sdf, "userConditions")
  varNamesConditions <- c()
  if(length(userConditions)>0) {
    for (i in c(1:length(userConditions))) {
      condition <- userConditions[[i]]
      varNamesConditions <- c(varNamesConditions, all.vars(condition))
    }
  }
  if(!inherits(sdf, "edsurvey.data.frame")) {
     varNamesConditions <- c()
  }
  
  # NOTE: you need to do this next part first so that, when you do just return what the user asked for,
  # NOTE: you can return to them the variables that they named and what the plausible values / weights
  
  #Using the conditions and default variables and varnames to read in data from LaF
  varNamesDefaults2 <- varNamesDefaults
  varNamesDefaults <- varNamesDefaults[which(varNamesDefaults %in% names(sdf))]
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
      if(length(varnamesAllConditions) == 0) break
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

  # this section is for retreiving the data from a LaF
  if(inherits(sdf, "edsurvey.data.frame")) {
    #Retrieve the LAF objects
    dataLaf <- sdf$data
    dataSchLaf <- sdf$dataSch
    
    #Retrieve filename
    filename <- sdf$fileDescription[['filename']]
    
    ndsl <- tolower(names(dataSchLaf))
    
    # figure out if any of the included variables are from the school table
    indexSchVars <- ndsl %in% c(varnamesTotal, schoolMergeVarSchool)
    if(sum(indexSchVars)>1) {
      # if so, add the merge var
      varnamesTotal <- c(varnamesTotal, schoolMergeVarStudent)
    }
    
    missingVars <- varnames[!varnames %in% names(sdf)]
    if(length(missingVars) >0 ) {
      stop(paste0("The following variable names are required for this call and are not on the incoming data ", paste(sQuote(missingVars), collapse=", "),"."))
    }
    
    indexVars <- (1:length(names(dataLaf)))[names(dataLaf) %in% varnamesTotal]

    # get the data using the indexes
    data <- dataLaf[,indexVars, drop=FALSE]
    names(data) <- tolower(names(data))
    if(!is.null(dataSchLaf)) {
      if(sum(indexSchVars)>1) {
        indexSchVars <- (1:length(ndsl))[indexSchVars]
        if(is.null(schoolMergeVarStudent)) {
          schoolMergeVarStudent <- "scrpsu"
          schoolMergeVarSchool <- "sscrpsu"
        }
        # get school level data
        datas <- dataSchLaf[,indexSchVars, drop=FALSE]
        names(datas) <- tolower(names(datas))
        data$oorder__zz11qq <- 1:nrow(data) # retain original order
        data <- merge(data, datas, by.x=schoolMergeVarStudent, by.y=schoolMergeVarSchool, all.x=TRUE, all.y=FALSE)
        data <- data[order(data$oorder__zz11qq),]
        data$oorder__zz11qq <- NULL
        varnamesTotal <- varnamesTotal[varnamesTotal!=schoolMergeVarSchool]
        varnames <- varnames[varnames!=schoolMergeVarSchool]
      }
    }
    
    #Apply labels and decimal values
    labelsFile <- rbind(sdf$fileFormat, sdf$fileFormatSchool)
    labelsFile <- labelsFile[order(labelsFile$Start),]
    decimals <- as.numeric(labelsFile$Decimal)
    variables <- tolower(labelsFile$variableName)
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
          temp <- strsplit(keysTemp[j],'=', fixed = TRUE)[[1]][2]
          if(temp == "" | is.na(temp)) {
            temp <- "label unknown"
          }
          values = c(values,temp)
          }
          
        labels[[variable]] <- list(keys = keys, values = values)
      }    
    }
  
    #applying labels to the data file
    labelsn <- names(labels)
    for(i in 1:length(labels)) {
      vari <- labelsn[i]
      if(vari %in% names(data)) {
        if(length(unique(labels[[i]]$keys)) != length(labels[[i]]$keys)) {
          warning("Duplicate variable label key i=", i, " in variable ",vari,".")
        }
        # in TIMSS files there are some variables that are real / integer and there is one omitted/invalid code
        if(length(labels[[i]]$values) == 1 && labels[[i]]$values == "OMITTED OR INVALID" && nchar(gsub("9","",labels[[i]]$keys)) == 0) {
          data[data[,vari] %in% labels[[i]]$keys,vari] <- NA
        } else {
          # this is a truly labeled file
          if(length(unique(labels[[i]]$values)) != length(labels[[i]]$values)) {
            tab <- table(labels[[i]]$values)
            dnames <- names(tab[tab > 1])
            needNewLabels <- labels[[i]]$values %in% dnames
            if(vari %in% varnames) {
              warning(paste0("Updating labels on ",sQuote(vari), " because there are multiples of the label ", sQuote(dnames), "."))  
            }
            labels[[i]]$values[needNewLabels] <- paste(labels[[i]]$values[needNewLabels], LETTERS[1:(sum(needNewLabels))], sep=":")
          }
          lvls <- labels[[i]]$keys
          lbls <- labels[[i]]$values
          if(includeNaLabel) {
            if(sum(is.na(data[,vari])) > 0) {
              lvls <- c(lvls, NA)
              lbls <- c(lbls, "(Missing)")
            }
          }
          data[data[,vari]=="" | is.na(data[,vari]),vari] <- NA 
          suppressWarnings(lvlsp <- as.numeric(lvls))
          suppressWarnings(dvi <- as.numeric(data[,vari]))
          suppressWarnings(lblsp <- as.numeric(lbls))
          if(sum(is.na(lvlsp)) - sum(is.na(lvls)) > 0 || sum(is.na(unique(dvi))) - sum(is.na(unique(data[,vari]))) >0 || sum(!is.na(lblsp)) >0) {
            data[,vari] <- factor(data[,vari], levels=lvls, labels=lbls, exclude=NULL)
          } else {
            data[,vari] <- lfactor(dvi, levels=lvlsp, labels=lbls, exclude=NULL)
          }
        } # end else for if(length(labels[[i]]$values) == 1 && labels[[i]]$values == "OMITTED OR INVALID" && nchar(gsub("9","",labels[[i]]$keys)) == 0)
      } else { # end if(vari %in% names(data))
        # you get here if there is a variable on the labels file but not in the data.
        # nothing to do
      }# end else for if(vari %in% names(data))
    } # for for(i in 1:length(labels))

    # apply omittedLevels when TRUE or equal to some levels
    # this code should execute on all of the variables when omittedLevels is TRUE
    flag <- FALSE # set to TRUE when using ommittedLevels in some capacity
    # lev variable is the levels that are being omitted
    if(omittedLevels == TRUE) {
      # NOTE: should deal with "OMITTED OR INVALID" for TIMSS
      lev <- unlist(sdf$omittedLevels) # here we know it is an edsurvey.data.frame
      flag <- TRUE
    }
    if(!is.logical(omittedLevels)) {
      lev <- omittedLevels
      flag <- TRUE
    }
    
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
    
    for(i in c(1:length(names.edsurvey.data.frame(sdf)))) {
      varn <- variables[i] # the variable being considered
      if(decimals[i]!=0 & varn %in% varnamesTotal) {
        # if it has a decimal conversion and is in the requested data
        data[,varn] <- data[,varn]/(10^decimals[i])
      }
    }
    
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
                     sQuote("defaultConditions"), " to ", dQuote("FALSE"), "."))
      defaultConditions <- FALSE
    }

    # then actually apply defaultConditions
    if(defaultConditions) {
      if(length(dConditions) > 0) {
        r <- eval(dConditions, data)
        data <- data[r,,drop=FALSE]
      }
    }
  
    #Apply user conditions
    if(length(userConditions)>0) {
      for (i in c(1:length(userConditions))) {
        condition <- userConditions[[i]]
        r <- eval(condition, data)
        data <- data[r,,drop=FALSE]
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
    # end portion run just when sdf is an edsurvey.data.frame
    missingVars <- varnames[!varnames %in% names(sdf)]
    if(length(missingVars) >0 ) {
      stop(paste0("The following variable names are required for this call and are not on the incoming data ", paste(sQuote(missingVars), collapse=", "),"."))
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
      lev <- tolower(lev)
      keep <- rep(0, nrow(data))
      for (i in 1:length(varnamesTotal)) {
        vari <- varnamesTotal[i]
        if(! vari %in% vars_exclude_omitted) {
          # omit data at these levels
          keep <- keep + (tolower(data[,vari]) %in% lev)
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
  if(!is.null(recode)) {
    # apply recodes
    for (i in 1:length(recode)){
      ni <- names(recode)[i]
      from <- recode[[i]]$from
      to <- recode[[i]]$to
      if (length(to) > 1) {
        stop(paste0("More than one 'To' value found in the ", sQuote(ni) ," element of the 'recode' argument."))
      }
      badFrom <- c()
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
          if(to %in% levs) { # if they to is a numeric
            # map it back to character
            if(!to %in% levs) {
              labs <- c(labs, "Unlabeled")
              levs <- c(levs, max(levs)+1)
            }
            to <- labs[levs==to]
          }
          if(!is.null(newto) && newto %in% levs) { # in lfactors, this is another way this could be not a new to value
            newto <- NULL
          }
          suppressWarnings(from1 <- as.numeric(from)) # numeric from variables
          from2 <- from # character from variables
          from2 <- from2[is.na(from1)]
          # numeric from variables
          from1 <- from1[!is.na(from1)]
          if(is.numeric(to)) {
            if(to %in% as.character(from1)) { # remove degenerate numeric recode
              from1 <- from1[!from1 %in% to]
            }
          }
          if(length(from1)>0) {
            tmp_numeric <- lfactors:::switchllevels(data[,ni])
            tmp[tmp_numeric %in% from1] <- to
            if(sum(!from1 %in% levs) > 0) {
              badfrom <- from1[!from1 %in% levs]
            }
            labs <- labs[!levs %in% from1]
            levs <- levs[!levs %in% from1]
          }
          if(length(from2)>0) {
            tmp[tmp %in% from2] <- to
            if(sum(!from2 %in% labs) > 0) {
              badfrom <- c(badFrom, from2[!from2 %in% labs])
            }
            levs <- levs[!labs %in% from2] 
            labs <- labs[!labs %in% from2] 
          }
          if(!is.null(newto)) {
            levs <- c(levs, max(levs)+1)
            labs <- c(labs, newto)
          }
          tmp2 <- rep(NA, length(tmp))
          for(i in 1:length(levs)) {
            tmp2[tmp %in% labs[i]] <- levs[i]
          }
          data[,ni] <- lfactor(tmp2, levels=levs, labels=labs)
        } else { # end if(inherits(data[,ni],"lfactor"))
          # it is a vanilla factor
          tmp[tmp %in% from] <- to
          if(sum(!from %in% labs) > 0) {
            badfrom <- from[!from %in% labs]
          }
          labs <- labs[!labs %in% from]
          if(!is.null(newto)) {
            labs <- c(labs, newto)
          }
          data[,ni] <- factor(data[,ni], levels=labs)
        }
      } else { # end if(inherits(data[,ni], "factor"))
        # for non-factors, this is incredibly simple
        if(sum(!from %in% data[,ni]) > 0) {
          badfrom <- from[!from %in% data[,ni]]
        }
        data[,ni][data[,ni] %in% from] <- to
      } # end else for if(inherits(data[,ni], "factor"))
      if(length(badFrom) > 0) {
        warning(paste0("When recoding, could not find the level(s) ",
                       paste(dQuote(badfrom), collapse=", "),
                       " in the variable ", ni, "."))
      }
    } # for (i in 1:length(recode))
  } # end if(!is.null(recode))
  
  if(addAttributes) {
    # the user requested a light.edsurvey.data.frame
    # these have everything in attributes (as well as the data already being read in)
    if(nrow(data) == 0) {
      warning("The requested data set has 0 rows.")
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
    class(data) <- c("light.edsurvey.data.frame", class(data))
    
    return(data)
  } #end if(addAttributes)
  if(nrow(data) == 0) {
    warning("The requested data set has 0 rows.")
  }
  data <- data[,varnames, drop=drop]
  data
}

# convToNum converts the variable to a number--if it will not cause data loss
convToNum <- function(x) {
  if(!is.character(x)) return(x)
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
