# build and edsurvey.data.frame from all of the requisite parts.
# There is no need for an S3 object to have a constructor, but having one
# can help programmers (and maintainers) know that every edsurvey.data.frame
# has all the correct elements.

#' @title EdSurvey Class Constructors and Helpers
#' @rdname edsurvey-class
#' @description Two new classes in \code{EdSurvey} are described in this section: the \code{edsurvey.data.frame}
#'              and \code{light.edsurvey.data.frame}. The \code{edsurvey.data.frame}
#'              class stores metadata about survey data, and data are stored on the
#'              disk (via the \code{LaF} package), allowing gigabytes of data to be used easily on a machine otherwise
#'              inappropriate for manipulating large datasets.
#'              The \code{light.edsurvey.data.frame} is typically generated
#'              by the \code{getData} function and stores the data in a
#'              \code{data.frame}.
#'              Both classes use attributes to manage metadata and allow
#'              for correct statistics to be used in calculating results; the
#'              \code{getAttributes} acts as an accessor for these attributes, whereas
#'              \code{setAttributes} acts as a mutator for the attributes.
#'              As a convenience, \code{edsurvey.data.frame}
#'              implements the \code{$} function to extract a variable.
#'
#' @param data an \code{edsurvey.data.frame} 
#' @param userConditions a list of user conditions that includes subsetting or recoding conditions
#' @param defaultConditions a list of default conditions that often are set for each survey
#' @param dataList a list of \code{dataListItem} objects to model the data structure of the survey 
#' @param weights a list that stores information regarding weight variables. See Details.
#' @param pvvars a list that stores information regarding plausible values. See Details.
#' @param subject a character that indicates the subject domain of the given data
#' @param year a character or numeric that indicates the year of the given data
#' @param assessmentCode a character that indicates the code of the assessment.
#'                       Can be \code{National} or \code{International}.
#' @param dataType a character that indicates the unit level of the main data.
#'                 Examples include \code{Student}, \code{teacher}, \code{school},
#'                 \code{Adult Data}.
#' @param gradeLevel a character that indicates the grade level of the given data
#' @param achievementLevels a list of achievement-level categories and cutpoints
#' @param omittedLevels a list of default omitted levels for the given data
#' @param survey a character that indicates the name of the survey
#' @param country a character that indicates the country of the given data
#' @param psuVar a character that indicates the PSU sampling unit variable. Ignored when weights have \code{psuVar} defined.
#' @param stratumVar a character that indicates the stratum variable. Ignored when weights have \code{stratumVar} defined.
#' @param jkSumMultiplier a numeric value of the jackknife coefficient (used in calculating the jackknife replication estimation)
#' @param recodes a list of variable recodes of the given data
#' @param validateFactorLabels a Boolean that indicates whether the \code{getData} function needs to validate factor variables
#' @param forceLower a Boolean; when set to \code{TRUE}, will automatically lowercase variable names
#' @param reqDecimalConversion a Boolean; when set to \code{TRUE}, a \code{getData} call will multiply the raw file value by a decimal multiplier
#' @param x an \code{edsurvey.data.frame}
#' @param i a character, the column name to extract
#' @param attribute a character, name of an attribute to get or set
#' @param value outside of the assignment context, new value of the given \code{attribute}
#' @param weightVar a character indicating the full sample weights
#' @param name a character vector of the column to edit
#'
#' @details
#'
#' The \code{weight} list has an element named after each weight variable name
#' that is a list with elements \code{jkbase} and \code{jksuffixes}. The
#' \code{jkbase} variable is a single character indicating the jackknife replicate
#' weight base name, whereas \code{jksuffixes} is a vector with one element for each
#' jackknife replicate weight. When the two are pasted together, they should form
#' the complete set of the jackknife replicate weights. The \code{weights} argument
#' also can have an attribute that is the default weight. If the primary sampling
#' unit and stratum variables change by weight, they also can be defined on the weight
#' list as \code{psuVar} and \code{stratumVar}. When this option is used, it overrides
#' the \code{psuVar} and \code{stratumVar} on the \code{edsurvey.data.frame},
#' which can be left blank. A weight must define only one of \code{psuVar}
#' and \code{stratumVar}.
#'
#' The \code{pvvars} list has an element for each subject or subscale score
#' that has plausible values. Each element is a list with a \code{varnames}
#' element that indicates the column names of the plausible values and an
#' \code{achievementLevel} argument that is a named vector of the 
#' achievement-level cutpoints.
#'
#'
#' @return
#' An object of class \code{edsurvey.data.frame} with the following elements:
#'
#' \emph{Elements that store data connections and data codebooks}
#'    \item{\code{dataList}}{a \code{list} object containing the surveys \code{dataListItem} objects}
#' \emph{Elements that store sample design and default subsetting information of the given survey data}
#'    \item{\code{userConditions}}{a list containing all user conditions, set using the \code{subset.edsurvey.data.frame} method}
#'    \item{\code{defaultConditions}}{the default subsample conditions}
#'    \item{\code{weights}}{a list containing the weights. See Details.}
#'    \item{\code{stratumVar}}{a character that indicates the default strata identification variable name in the data. Often used in Taylor series estimation.}
#'    \item{\code{psuVar}}{a character that indicates the default PSU (sampling unit) identification variable name in the data. Often used in Taylor series estimation.}
#'    \item{\code{pvvars}}{a list containing the plausible values. See Details.}
#'    \item{\code{achievementLevels}}{default achievement cutoff scores and names. See Details.}
#'    \item{\code{omittedLevels}}{the levels of the factor variables that will be omitted from the \code{edsurvey.data.frame}}
#' \emph{Elements that store descriptive information of the survey}
#'    \item{\code{survey}}{the type of survey data}
#'    \item{\code{subject}}{the subject of the data}
#'    \item{\code{year}}{the year of assessment}
#'    \item{\code{assessmentCode}}{the assessment code}
#'    \item{\code{dataType}}{the type of data (e.g., \code{student} or \code{school})}
#'    \item{\code{gradeLevel}}{the grade of the dataset contained in the \code{edsurvey.data.frame}}
#' @section EdSurvey Classes:
#' \code{edsurvey.data.frame} is an object that stores connection to data on the
#' disk along with important survey sample design information.
#'
#' \code{edsurvey.data.frame.list} is a list of \code{edsurvey.data.frame}
#' objects. It often is used in trend or cross-regional analysis in the
#' \code{\link{gap}} function. See \code{\link{edsurvey.data.frame.list}} for
#' more information on how to create an \code{edsurvey.data.frame.list}. Users
#' also can refer to the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Trend.pdf}{\emph{Using EdSurvey for Trend Analysis}}
#' for examples.
#'
#' Besides \code{edsurvey.data.frame} class, the \code{EdSurvey} package also
#' implements the \code{light.edsurvey.data.frame} class, which can be used by both
#' \code{EdSurvey} and non-\code{EdSurvey} functions. More particularly,
#' \code{light.edsurvey.data.frame} is a \code{data.frame} that has basic
#' survey and sample design information (i.e., plausible values and weights), which
#' will be used for variance estimation in analytical functions. Because it
#' also is a base R \code{data.frame}, users can apply base R functions for
#' data manipulation.
#' See the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-getData.pdf}{\emph{Using the \code{getData} Function in EdSurvey}}
#' for more examples.
#'
#' Many functions will remove attributes from a data frame, such as
#' a \code{light.edsurvey.data.frame}, and the
#' \code{\link{rebindAttributes}} function can add them back.
#'
#' Users can get a \code{light.edsurvey.data.frame} object by using the
#' \code{\link{getData}} method with \code{addAttributes=TRUE}.
#'
#' @section Basic Methods for EdSurvey Classes:
#' \emph{Extracting a column from an \code{edsurvey.data.frame}}
#'
#' Users can extract a column from an \code{edsurvey.data.frame} object using \code{$} or \code{[]} like a normal data frame.
#'
#' \emph{Extracting and updating attributes of an object of class \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}}
#'
#' Users can use the \code{getAttributes} method to extract any attribute of
#' an \code{edsurvey.data.frame} or a \code{light.edsurvey.data.frame}. 
#' A \code{light.edsurvey.data.frame} will not have attributes related to data connection
#' because data have already been read in memory.
#'
#' If users want to update an attribute (i.e., \code{omittedLevels}), they can
#' use the \code{setAttributes} method.
#'
#' @seealso \code{\link{rebindAttributes}}
#' @example \man\examples\edsurvey-class.R
#'
#' @importFrom LaF close
#' @importFrom utils capture.output
#' @author Tom Fink, Trang Nguyen, and Paul Bailey
#' @export
edsurvey.data.frame <- function(userConditions,
                                defaultConditions,
                                dataList=list(),
                                weights,
                                pvvars,
                                subject,
                                year,
                                assessmentCode,
                                dataType,
                                gradeLevel,
                                achievementLevels,
                                omittedLevels,
                                survey,
                                country,
                                psuVar,
                                stratumVar,
                                jkSumMultiplier,
                                recodes=NULL,
                                validateFactorLabels=FALSE,
                                forceLower=TRUE,
                                reqDecimalConversion=TRUE) {
  if (is.list(achievementLevels)) {
    achievementLevels <- lapply(achievementLevels, function(l) {
      sort(l)})
  } else {
    # In the international assessment TALIS, there is no achievement level, account for that here
    if(!is.null(achievementLevels)) {
      achievementLevels <- sort(achievementLevels)
      # names can't be missing, but they can be length 0. Figure out if that is the case.
      if(min(nchar(names(achievementLevels)))==0) {
        stop(paste0("The argument ", sQuote("achievementLevels"), " must have nonmissing names for each level."))
      }
      for (pvi in 1:length(pvvars)) {
        # setup achievement levels for all plausible value variables
        temp = list(varnames = pvvars[[pvi]]$varnames)
        temp$achievementLevel <- achievementLevels
        pvvars[[names(pvvars)[pvi]]] <- temp
      }
    }

  } #closes else statment following if (is.list(achievementLevels))

  dim0 <- c(0,0) #temporary holder until we create the edsurvey.data.frame, then we will update it

  if(forceLower) {
    i <- 1
    for(dl in dataList){
      names(dl$lafObject) <- tolower(names(dl$lafObject))
      dl$fileFormat$variableName <- tolower(dl$fileFormat$variableName)
      dl$mergeVars <- tolower(dl$mergeVars)
      dl$parentMergeVars <- tolower(dl$parentMergeVars)

      dataList[[i]] <- dl
      i <- i + 1
    }
  }

  res <- list(userConditions=userConditions,
              defaultConditions=defaultConditions,
              dataList=dataList,
              weights=weights,
              pvvars=pvvars,
              subject=subject,
              year=year,
              assessmentCode=assessmentCode,
              dataType=dataType,
              gradeLevel=gradeLevel,
              achievementLevels=achievementLevels,
              omittedLevels=omittedLevels,
              survey=survey,
              country=country,
              psuVar=psuVar,
              stratumVar=stratumVar,
              jkSumMultiplier=jkSumMultiplier,
              recodes=recodes,
              dim0=c(0,0), #update after it's reclassed to an edsurvey.data.frame
              validateFactorLabels=validateFactorLabels,
              reqDecimalConversion=reqDecimalConversion)
  # form cache, first grab dim
  ROWID <- 1
  # make cache
  res$cache <- data.frame(ROWID=ROWID)
  class(res) <- "edsurvey.data.frame"
  # use getData on this, as of yet incomplete edsurvey.data.frame
  # to get row IDs
  # supressWarnings because it is just about nrow
  suppressWarnings(gd0 <- getData(res, varnames=colnames(res)[1], dropUnusedLevels=FALSE, omittedLevels=FALSE, defaultConditions=FALSE))
  suppressWarnings(gd <- getData(res, varnames=colnames(res)[1], dropUnusedLevels=FALSE, omittedLevels=FALSE))
  class(res) <- "list"
  # update dim0
  nrow <- nrow(gd)
  cache <- data.frame(ROWID=1:nrow(gd0),
                      DEFAULT=1:nrow(gd0) %in% rownames(gd))
  # change back to a list to allow assignment of the cache
  res$cache <- cache
  # return to edsurvey.data.frame
  class(res) <- "edsurvey.data.frame"
  res$dim0 <- c(nrow(res), length(colnames(res)))
  # we discovered that not closing a LaF connections leads to memory leaks.
  # so, close all of the connections when it is complete (constructed).
  for(item in dataList){
    LaF::close(item$lafObject)
  }
  if(any(!c(psuVar, stratumVar) %in% c("JK1", colnames(res)))) {
    warning("Cannot find both PSU and Stratum variables on data. Taylor series variance estimation will not be possible.")
  }
  return(res)
}

#' @rdname edsurvey-class
#' @method $ edsurvey.data.frame
#' @export
"$.edsurvey.data.frame" <- function(x, i) {
  # for the attributes
  if (i %in% names(x)) {
    return(x[[i]])
  }
  
  success <- tryCatch({z <- getData(x, varnames=i,dropUnusedLevels=FALSE, omittedLevels=FALSE,drop=TRUE)
                        TRUE
                      },error = function(e){
                        warning(paste0("No object or data variable found named ", sQuote(i)))
                        FALSE
                      })
                  
  if(success){
    return(z)
  }else{
    return(invisible(NULL))
  }
}

# @note The \code{dataListItem} function is a useful constructor for building the individual \code{dataList} heirarchy data model components of each \code{dataList} object of an \code{edsurvey.data.frame}.
#        It's recommended to always use this constructor, as it applies default values, calculations, validation checks, and to help for future functionality.  
# @param lafObject an LaF object file that stores the data file connection and information.
# @param fileFormat a data.frame of the defined file format specifications of the data file.  See \code{edsurvey.data.frame} Details.
# @param levelLabel a character to describe the name of this \code{dataListItem}.  Must be unique from other \code{dataListItem} objects in the same \code{edsurvey.data.frame}.
# @param forceMerge a logical to determine if a \code{dataListItem} must always be merged in \code{getData}. A value \code{TRUE} causes the \code{dataListItem} to always be merged.
#                   Default is \code{FALSE}.
# @param parentMergeLevels a character which is of equal lengths to the \code{parentMergeVars} paremeter specifying the \code{levelLabel} of the matching \code{parentMergeVars} variable.
# @param parentMergeVars a character which is of equal lengths to the \code{parentMergeLevels} and \code{mergeVars} parameter specifying the variable name the corresponding \code{parentMergeLevels}'s \code{dataListItem}
#                        to which will be merged to the \code{mergeVar} variable in this \code{dataListItem}.
# @param mergeVars a character which is of equal lengths to the \code{parentMergeLevels} and \code{parentMergeVars} parameter specifying this \code{dataListItem} variable name
#                        to which will be merged to the corresponding \code{parentMergeVars} variable in the level defined by \code{parentMergeLevels}.
# @param ignoreVars a character of variable name(s) to be ignored when testing if a \code{dataListItem} level should be merged or not.  Useful when \code{mergeVars} have matching names across \code{dataListItems}.
# @param isDimLevel a logical value of the \code{dataListItem} for which should be considered the 'full data N'.  Only one \code{dataListItem} in an \code{edsurvey.data.frame} should have a
#                   value of \code{TRUE}.  Default value is \code{FALSE}.
dataListItem <- function(lafObject,
                         fileFormat,
                         levelLabel="",
                         forceMerge=FALSE,
                         parentMergeLevels=NULL,
                         parentMergeVars=NULL,
                         mergeVars=NULL,
                         ignoreVars=NULL,
                         isDimLevel=FALSE) {

  if(class(lafObject)!="laf"){
    stop(paste0("The ", dQuote("lafObject"), " must be a LaF object utilizing the LaF Package."))
  }

  if (length(parentMergeLevels)!=length(parentMergeVars)){
    stop(paste0("The lengths of ", dQuote("parentMergeLevels"), " and ", dQuote("parentMergeVars"), " must be equal."))
  }

  if (length(parentMergeVars)!=length(mergeVars)){
    stop(paste0("The lengths of ", dQuote("parentMergeVars"), " and ", dQuote("mergeVars"), " must be equal."))
  }
  res <- list(lafObject = lafObject,
              fileFormat = fileFormat,
              levelLabel = levelLabel,
              forceMerge = forceMerge,
              parentMergeLevels = parentMergeLevels,
              parentMergeVars = parentMergeVars,
              mergeVars = mergeVars,
              ignoreVars = ignoreVars,
              rowCount = nrow(lafObject[,1]),
              colCount = ncol(lafObject[1,]),
              isDimLevel = isDimLevel)

  return(res)
}



#' @name subset
#' @title EdSurvey Subset
#' @aliases subset.edsurvey.data.frame.list subset.light.edsurvey.data.frame
#'
#' @description Subsets an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list},
#' or a \code{light.edsurvey.data.frame}.
#'
#' @param x an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list},
#'          or a \code{light.edsurvey.data.frame}
#' @param subset a logical expression indicating elements or rows to keep
#' @param inside set to \code{TRUE} to prevent the \code{substitute} condition
#'               from being called on it (see Details)
#' @param ... not used; included only for compatibility
#' @details Any variables defined on condition that are not references
#' to column names on the
#' \code{edsurvey.data.frame} and are part of the environment where
#' \code{subset.edsurvey.data.frame} was called will be evaluated
#' in the environment from which \code{subset.edsurvey.data.frame} was called.
#' Similar to the difficulty of using subset within a function call because of
#' the call to substitute on condition,
#' this function is difficult to use (with \code{inside} set to the default value of
#' \code{FALSE}) inside another function call.
#' See Examples for how to call this function from within another function.
#'
#' @return an object of the same class as \code{x}
#' @references
#' Wickham, H. (2014). \emph{Advanced R}. Boca Raton, FL: Chapman & Hall/CRC.
#' 
#' @author Paul Bailey and Trang Nguyen
#' @example man\examples\subset.edsurvey.data.frame.R
#' @method subset edsurvey.data.frame
#' @export
subset.edsurvey.data.frame <- function(x, subset, ..., inside=FALSE) {
  env <- parent.frame(n=2)
  
  if(!inherits(x, c("edsurvey.data.frame"))) {
    stop(paste0("The argument ", sQuote("x"), " must be an edsurvey.data.frame."))
  }

  if(inside) {
    if(inherits(subset, "character")) {
      subset <- parse(text=subset)[[1]]
    }
    condition_call <- subset
  } else {
    # if there is a variable that is not in the data.frame, substitute any
    # value found in the parent.frame() right now.
    # This way, if the user adjusts a variable used in the subset, it will
    # have the value they would have expected from
    # when they called subset and the condition will not change as that
    # variable is updated.
    # add it to the user conditions

    # parse the condition
    # substitute in variables that are available in the current environment
    condition_call <- substitute(subset)
    iparse <- function(ccall, env) {
      # for each element
      for(i in 1:length(ccall)) {
        # if it is a name
        if(inherits(ccall[[i]], "name")) {
          ccall_c <- as.character(ccall[[i]])
          # if it is not in the data and is in the parent.frame, then substitue it now.
          if((! ccall_c %in% colnames(x)) & (ccall_c %in% ls(envir=env)) ) {
            if (ccall[[i]] == "%in%" || is.function(ccall[[i]])) {
              ev <- eval(substitute(ccall[[i]]), parent.frame())  
            } else {
              ev <- eval(ccall[[i]], parent.frame())
            } #end of if/esle statment: if ccall[[i]] == "%in%" || is.function(ccall[[i]])
            ccall[[i]] <- ev
          } # End of if statment: if (! ccall_c %in% colnames(x$data)) & (ccall_c %in% ls(envir=env)) 
        } # end if(inherits(ccall[[i]], "name"))
        if(inherits(ccall[[i]], "call")) {
          # if this is a call, recursively parse that
          ccall[[i]] <- iparse(ccall[[i]], env)
          # if no vars are in colnames(x), evaluate the call right now
          if( any(!all.vars(ccall[[i]]) %in% colnames(x)) ) {
            # unclear if this tryCatch does anything, but it seems wise to keep it
            tryCatch(res <- eval(ccall[[i]]),
                     error=function(e) {
                       co <- capture.output(print(ccall[[i]]))
                       stop(paste0("The condition ", dQuote(co), " cannot be evaluated."))
                     })
            # the condition resolves to NULL if, for example, it references
            # a list element that is not on the list. But the list itself is
            # on the parent.frame.
            if(is.null(res)) {
              co <- capture.output(print(ccall[[i]]))
              stop(paste0("Condition ", dQuote(co), " cannot be evaluated."))
            }
            ccall[[i]] <- res
          }
        } #end of if statment: if inherits(ccall[[i]], "call")
      } # end of for loop: i in 1:length(ccall)
      ccall
    } # End of fucntion: iparse
    condition_call <- iparse(condition_call, env)
    #condition_call <- iparse(condition_call, parent.frame())
  } # Enf of if esle statmet: if imside is true 
  # apply filter
  x[["userConditions"]] <- c(x[["userConditions"]],list(condition_call))
  # test filter
  tryCatch(getData(x, colnames(x)[1]),
           error=function(e) {
             subsetVars <- all.vars(condition_call)
             for (v in subsetVars) {
               if (!v %in% colnames(x)) {
                 stop("Required variable ", sQuote(v), " is not found in the incoming data or on the environment.")
               }
             }
           },
           warning=function(w) {} )
  x
} # end of fuction subset.edsurvey.data.frame

#' @method [[ edsurvey.data.frame
#' @export
"[[.edsurvey.data.frame" <- function(x, i, j, ...) {
  # check for $ calls to list elements
  if(length(i) == 1) {
    if(i %in% names(x)) {
      class(x) <- "list"
      return(x[[i]])
    }
  }
  if(is.numeric(i)) {
    i <- colnames(x)[i]
  }
  suppressWarnings(
    z <- getData(x, varnames=i,
                 dropUnusedLevels=FALSE, omittedLevels=FALSE,
                 addAttributes=TRUE)
    )
  z[j, ]
}

#' @method [ edsurvey.data.frame
#' @export
"[.edsurvey.data.frame" <- function(x, i, j, ...) {
  # check for $ calls to list elements
  if(length(j) == 1) {
    if(j %in% names(x)) {
      class(x) <- "list"
      return(x[[j]])
    }
  }
  if(is.numeric(j)) {
    j <- colnames(x)[j]
  }
  suppressWarnings(
    z <- getData(x, varnames=j,
                 dropUnusedLevels=FALSE, omittedLevels=FALSE,
                 addAttributes=TRUE)
    )
  z[i, ]
}

#' @rdname edsurvey-class
#' @method $<- edsurvey.data.frame
#' @export
"$<-.edsurvey.data.frame" <- function(x, name, value) {
  cl <- match.call()
  cl2 <- cl
  if(name %in% names(x)) {
    cl0 <- class(x)
    class(x) <- "list"
    x[[name]] <- value
    class(x) <- cl0
    invisible(x)
  } else {
    cl0 <- class(x)
    class(x) <- "list"
    x$cache[x$cache$DEFAULT,name] <- value
    class(x) <- cl0
    invisible(x)
  }
}
