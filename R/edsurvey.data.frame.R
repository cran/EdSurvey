# build and edsurvey.data.frame from all of the requisite parts.
# There is no need for an S3 object to have a constructor, but having one
# can help programmers (and maintainers) know that every edsurvey.data.frame
# has all the correct elements.

#' @title EdSurvey Class Constructors
#' @rdname edsurvey-class
#' @description Two new classes in EdSurvey are described in this section: the \code{edsurvey.data.frame}
#'              and \code{light.edsurvey.data.frame}. The \code{edsurvey.data.frame}
#'              class stores metadata about survey data, and data are stored on the
#'              disk (via the \code{LaF} package), allowing GB of data to be used easily on a machine otherwise
#'              inappropriate for manipulating large datasets.
#'              The \code{light.edsurvey.data.frame} is typically generated
#'              by the \code{getData} function and stores the data in a
#'              \code{data.frame}.
#'              Both of the classes use attributes to manage metadata and allow
#'              for correct statistics to be used in calculating results; the
#'              \code{getAttributes} acts as an accessor for these attributes, while
#'              \code{setAttributes} acts as a mutator for the attributes.
#'              As a convenience, \code{edsurvey.data.frame}
#'              implements the \code{$} function to extract a variable.
#'
#' @param data an \code{edsurvey.data.frame} 
#' @param userConditions a list of user conditions that includes subsetting or recoding conditions
#' @param defaultConditions a list of default conditions that are often set for each survey
#' @param dataList a list of \code{dataListItem} objects to model the data structure of the survey 
#' @param weights a list that stores information regarding weight variables. See Details.
#' @param pvvars a list that stores information regarding plausible values. See Details.
#' @param subject a character that indicates subject domain of the given data
#' @param year a character or numeric that indicates year of the given data
#' @param assessmentCode a character that indicates the code of the assessment.
#'                       Can be \dQuote{National} or \dQuote{International}.
#' @param dataType a character that indicates the unit level of the main data.
#'                 Examples include \dQuote{Student}, \dQuote{teacher}, \dQuote{school},
#'                 \dQuote{Adult Data}.
#' @param gradeLevel a character that indicates grade level of the given data
#' @param achievementLevels a list of achievement-level categories and cutpoints
#' @param omittedLevels a list of default omitted levels for the given data
#' @param survey a character that indicates the name of the survey
#' @param country a character that indicates the country of the given data
#' @param psuVar a character that indicates the PSU sampling unit variable. Ignored when weights have psuVar defined.
#' @param stratumVar a character that indicates the stratum variable. Ignored when weights have stratumVar defined.
#' @param jkSumMultiplier a numeric value of the jackknife coefficient (used in calculating the jackknife replication estimation)
#' @param recodes a list of variable recodes of the given data
#' @param validateFactorLabels a Boolean that indicates whether the \code{getData} function needs to validate factor variables
#' @param forceLower a Boolean; when set to \code{TRUE}, will automatically lowercase variable names
#' @param reqDecimalConversion a Boolean; when set to \code{TRUE}, a \code{getData} call will multiply the raw file value by a decimal multiplier
#' @param x an \code{edsurvey.data.frame}
#' @param i a character, the column name to extract
#' @param attribute a character, name of an attribute to get or set
#' @param value new value of the given \code{attribute}
#'
#' @details
#'
#' The \code{weight} list has an element named after each weight variable name
#' that is a list with elements \code{jkbase} and \code{jksuffixes}. The
#' \code{jkbase} variable is a single character indicating the jackknife replicate
#' weight base name, while \code{jksuffixes} is a vector with one element for each
#' jackknife replicate weight. When the two are pasted together, they should form
#' the complete set of jackknife replicate weights. The \code{weights} argument
#' can also have an attribute that is the default weight. If the primary sampling
#' unit and stratum variables change by weight, they can also be defined on the weight
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
#' \describe{
#'    \item{dataList}{a \code{list} object containing the surveys \code{dataListItem} objects}
#' }
#' \emph{Elements that store sample design and default subsetting information of the given survey data}
#' \describe{
#'    \item{userConditions}{a list containing all user conditions, set using the \code{subset.edsurvey.data.frame} method}
#'    \item{defaultConditions}{the default subsample conditions}
#'    \item{weights}{a list containing the weights. See Details.}
#'    \item{stratumVar}{a character that indicates the default strata identification variable name in the data. Often used in Taylor series estimation.}
#'    \item{psuVar}{a character that indicates the default PSU (sampling unit) identification variable name in the data. Often used in Taylor series estimation.}
#'    \item{pvvars}{a list containing the plausible values. See Details.}
#'    \item{achievementLevels}{default achievement cutoff scores and names. See Details.}
#'    \item{omittedLevels}{the levels of the factor variables that will be omitted from the \code{edsurvey.data.frame}}
#' }
#' \emph{Elements that store descriptive information of the survey}
#' \describe{
#'    \item{survey}{the type of survey data}
#'    \item{subject}{the subject of the data}
#'    \item{year}{the year of assessment}
#'    \item{assessmentCode}{the assessment code}
#'    \item{dataType}{the type of data (e.g., \dQuote{student} or \dQuote{school})}
#'    \item{gradeLevel}{the grade of the dataset contained in the \code{edsurvey.data.frame}}
#'  }
#' @section EdSurvey Classes:
#' \code{edsurvey.data.frame} is an object that stores connection to data on the
#' disk along with important survey sample design information.
#'
#' \code{edsurvey.data.frame.list} is a list of \code{edsurvey.data.frame}
#' objects. It is often used in trend or cross-regional analysis in the
#' \code{\link{gap}} function. See \code{\link{edsurvey.data.frame.list}} for
#' more information on how to create an \code{edsurvey.data.frame.list}. Users
#' can also refer to the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Trend.pdf}{Using EdSurvey for Trend Analysis}
#' for examples.
#'
#' Besides \code{edsurvey.data.frame} class, \code{EdSurvey} package also
#' implements \code{light.edsurvey.data.frame} class, which can be used by both
#' EdSurvey and non-EdSurvey functions. More particularly, \
#' \code{light.edsurvey.data.frame} is a \code{data.frame} that also has basic
#' survey and sample design information (i.e., plausible values and weights), which
#' will be used for variance estimation in analytical functions. Because it is
#' also a base R \code{data.frame}, users can also apply base R functions for
#' data manipulation.
#' See vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-getData.pdf}{getData}
#' for more examples.
#'
#' Many functions will remove attributes from a data frame, such as
#' a \code{light.edsurvey.data.frame}, and the
#' \code{\link{rebindAttributes}} function can add them back.
#'
#' Users can get a \code{light.edsurvey.data.frame} object by using
#' \code{\link{getData}} method with \code{addAttributes=TRUE}.
#'
#' @section Basic Methods for EdSurvey Classes:
#' \emph{Extracting a column from an \code{edsurvey.data.frame}}
#'
#' Users can extract a column from an \code{edsurvey.data.frame} object using \code{$} or \code{[]} like a normal data frame.
#'
#' \emph{Extracting and updating attributes of an object of class \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}}
#'
#' Users can use \code{getAttributes} method to extract any of the attributes of
#' an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}. Note that
#' a \code{light.edsurvey.data.frame} will not have attributes related to data connection
#' because data have already been read in memory.
#'
#' If users want to update an attribute (i.e., \code{omittedLevels}), they can
#' use the \code{setAttributes} method.
#'
#' @seealso \code{\link{rebindAttributes}}
#' @examples
#' # read in the example data (generated, not real student data)
#' sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))
#'
#' # Run a base R function on a column of edsurvey.data.frame
#' table(sdf$dsex)
#'
#' # Extract default omitted levels of NAEP primer data
#' getAttributes(sdf, "omittedLevels") #[1] "Multiple" NA         "Omitted"
#'
#' # Update default omitted levels of NAEP primer data
#' sdf <- setAttributes(sdf, "omittedLevels", c("Multiple", "Omitted", NA, "(Missing)"))
#' getAttributes(sdf, "omittedLevels") #[1] "Multiple"  "Omitted"   NA          "(Missing)"
#' @importFrom LaF close
#' @author Tom Fink, `Trang Nguyen, and Paul Bailey
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
  class(res) <- "edsurvey.data.frame"

  res$dim0 <- dim(res)

  # we discovered that not closing a LaF connections leads to memory leaks.
  # so, close all of the connections when it is complete (constructed).
  for(item in dataList){
    LaF::close(item$lafObject)
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
  suppressWarnings(
    z <- getData(x, varnames=i,
                 dropUnusedLevels=FALSE, omittedLevels=FALSE,drop=TRUE)
  )
  return(z)
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
