# build and edsurvey.data.frame from all of the requisite parts.
# There is no need for an S3 object to have a constructor, but having one
# can help programmers (and maintainers) know that every edsurvey.data.frame 
# has all the correct elements.

#' @title EdSurvey Class Constructors
#' @rdname edsurvey-class
#' @description Two new classes in EdSurvey are described in this section: the \code{edsurvey.data.frame}
#'              and \code{light.edsurvey.data.frame}. The \code{edsurvey.data.frame}
#'              class stores metadata about survey data and data is stored on the
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
#' @param userConditions a list of user conditions that includes subsetting or recoding conditions
#' @param defaultConditions a list of default conditions that are often set for each survey
#' @param data in the \code{edsurvey.data.frame} constructor, this is an \code{LaF} object
#'             that connects to the main data, often at the student level. For
#'             \code{getAttributes} and \code{setAttributes}, this argument is
#'             an \code{edsurvey.data.frame} or \code{light.edusrvey.data.frame}.
#' @param dataSch an \code{LaF} object that connects to the school-level data (optional)
#' @param dataTch an \code{LaF} object that connects to the teacher-level data (optional)
#' @param dataListMeta a list that stores variables that can be used to link school-level and teacher-level
#'                     data to the main data. See Details.
#' @param weights a list that stores information regarding weight variables. See Details.
#' @param pvvars a list that stores information regarding plausible values. See Details.
#' @param subject a character that indicates subject domain of the given data
#' @param year a character or numeric that indicates year of the given data
#' @param assessmentCode a character that indicates the code of the assessment.
#'                       Can be \dQuote{National} or \dQuote{International}.
#' @param dataType a character that indicates the unit level of the main data.
#'                 Examples include dQuote{Student}, \dQuote{teacher}, \dQuote{school},
#'                 \dQuote{Adult Data}.
#' @param gradeLevel a character that indicates grade level of the given data
#' @param achievementLevels a list of achievement level categories and cutpoints
#' @param omittedLevels a list of default omitted levels for the given data
#' @param fileFormat a \code{data.frame} that stores codebook information for the main data. See Details.
#' @param fileFormatSchool a \code{data.frame} that stores codebook information for the school-level data (if exists). See Details.
#' @param fileFormatTeacher a \code{data.frame} that stores codebook information for the teacher-level data (if exists). See Details.
#' @param survey a character that indicates the name of the survey
#' @param country a character that indicates the country of the given data
#' @param psuVar a character that indicates the PSU sampling unit variable. Ignored when weights have psuVar defined.
#' @param stratumVar a character indicates the stratum variable. Ignored with weights have stratumVar defined.
#' @param jkSumMultiplier a numeric value of the jackknife coefficient (used in calculating the jackknife replication estimation)
#' @param recodes a list of variable recodes of the given data
#' @param validateFactorLabels a Boolean that indicates whether the \code{getData} function needs to validate factor variables
#' @param forceLower a Boolean; when set to \code{TRUE}, will automatically lowercase variable names
#' @param x an \code{edsurvey.data.frame}
#' @param i a character, the column name to extract
#' @param attribute a character, name of an attribute to get or set
#' @param value new value of the given \code{attribute}
#'
#' @details 
#' The \code{dataListMeta} argument is a list with an element \code{student} that is also a list. 
#' Each element of the \code{student} list is another dataset name 
#' (\code{teacher} or \code{school}) that indicates the variables used to link
#' the student file to those files. The merge variables are 
#' shown with a caret character (\dQuote{\code{^}}) between them. The first variable
#' is the name of the merge variable on the student file, and the second variable
#' is the name of the merge variable on the school file. When multiple variables
#' are used to merge, a semicolon can separate pairs of variables; e.g.,
#' \code{student=list(school="varA^varY;varB^varZ")} would indicate that the student file 
#' can be merged to the school file using the \code{varA} and \code{varB}
#' variables from the student file to merge it to \code{varY} and \code{varZ},
#' respectively, on the school file.
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
#' \code{achievementLevel} argument that is a named vector of the achievement
#' level cut points. 
#'
#' The \code{fileFormat} arguments are data frames that have the following columns:
#' \describe{
#' \item{variableName}{name of the variable. Changed to lower case by the
#'                     constructor if \code{forceLower=TRUE}.}
#' \item{Start}{start column of the data}
#' \item{End}{end column of the data}
#' \item{Width}{number of characters wide the data is}
#' \item{Decimal}{power of 10 that the data should be divided by}
#' \item{Labels}{brief description of the variable}
#' \item{labelValues}{an caret (\dQuote{\code{^}}) delimited list of label
#'                    value pairs, each of which is equal delimited (\dQuote{\code{=}})
#'                    as \code{code=value}. For example, the string \dQuote{1=true^2=false^3=invalid}
#'                    would result in values of 1 being labeled \dQuote{true}, values
#'                    2 being labeled \dQuote{false}, and values of 3 being labeled \dQuote{invalid}.}
#' \item{dataType}{one of \dQuote{character}, \dQuote{numeric}, or \dQuote{integer}}
#' \item{Weights}{Boolean set to \code{TRUE} to indicate that the column is
#'                a full sample (not replicate) weight column}
#' }
#'
#' @return 
#' An object of class \code{edsurvey.data.frame} with the following elements:
#' 
#' \emph{Elements that store data connections and data codebooks}
#' \describe{
#'    \item{data}{an \code{LaF} object containing a connection to the student dataset on disk}
#'    \item{dataSch}{an \code{LaF} object containing a connection to the school dataset on disk if exists. If not, will be \code{NULL}.}
#'    \item{dataTch}{an \code{LaF} object containing a connection to the teacher dataset on disk if exists. If not, will be \code{NULL}.}
#'    \item{fileFormat}{a \code{data.frame} containing the format of the file in the \code{data} parameter. See Details.}
#'    \item{fileFormatSchool}{a \code{data.frame} containing the format of the file in the \code{dataSch} parameter. See Details.}
#'    \item{fileFormatTeacher}{a \code{data.frame} containing the format of the file in the \code{dataTch} parameter. See Details.}
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
#'  vignette titled
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
#' because data has already been read in memory.
#' 
#' If users want to update an attribute (i.e., \code{omittedLevels}), users can
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
                                data, 
                                dataSch,
                                dataTch,
                                dataListMeta,
                                weights, 
                                pvvars, 
                                subject, 
                                year, 
                                assessmentCode, 
                                dataType, 
                                gradeLevel, 
                                achievementLevels, 
                                omittedLevels, 
                                fileFormat, 
                                fileFormatSchool,
                                fileFormatTeacher,
                                survey,
                                country,
                                psuVar,
                                stratumVar,
                                jkSumMultiplier,
                                recodes=NULL,
                                validateFactorLabels=FALSE,
                                forceLower=TRUE) {
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
  
  #determine full dataset size
  if(!is.null(dataTch)){
    dim0 <- c(nrow(dataTch[,1]),
              length(unique(c(names(data), names(dataSch), names(dataTch)))))
  }else{
    dim0 <- c(nrow(data[,1]),
              length(unique(c(names(data), names(dataSch), names(dataTch)))))
  }
  if(forceLower) {
    names(data) <- tolower(names(data))
    fileFormat$variableName <- tolower(fileFormat$variableName)
    if(!is.null(dataSch)){
      names(dataSch) <- tolower(names(dataSch))
      dataListMeta$student$school <- tolower(dataListMeta$student$school)
      fileFormatSchool$variableName <- tolower(fileFormatSchool$variableName)
    }
    if(!is.null(dataTch)){
      names(dataTch) <- tolower(names(dataTch))
      dataListMeta$student$teacher <- tolower(dataListMeta$student$teacher)
      fileFormatTeacher$variableName <- tolower(fileFormatTeacher$variableName)
    }
  }
  res <- list(userConditions=userConditions, 
              defaultConditions=defaultConditions, 
              data=data, 
              dataSch=dataSch, 
              dataTch=dataTch,
              dataListMeta=dataListMeta,
              weights=weights, 
              pvvars=pvvars, 
              subject=subject, 
              year=year, 
              assessmentCode=assessmentCode, 
              dataType=dataType, 
              gradeLevel=gradeLevel, 
              achievementLevels=achievementLevels, 
              omittedLevels=unlist(omittedLevels), 
              fileFormat=fileFormat, 
              fileFormatSchool=fileFormatSchool, 
              fileFormatTeacher=fileFormatTeacher,
              survey=survey,
              country=country,
              psuVar=psuVar,
              stratumVar=stratumVar,
              jkSumMultiplier=jkSumMultiplier,
              recodes=recodes,
              dim0=dim0,
              validateFactorLabels=validateFactorLabels)
  class(res) <- "edsurvey.data.frame"
  
  # we discovered that not closing a LaF connections leads to memory leaks.
  # so, close all of the connections when it is complete (constructed).
  if(!is.null(data)){LaF::close(data)}
  if(!is.null(dataSch)){LaF::close(dataSch)}
  if(!is.null(dataTch)){LaF::close(dataTch)}
  
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
