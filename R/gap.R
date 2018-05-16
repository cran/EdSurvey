#' @title Gap Analysis
#'
#' @description Compares the average levels of a variable between two groups
#'              that potentially share members.
#'
#' @param variable a character indicating the variable to be compared,
#'                 potentially with a subject scale or subscale
#' @param data     an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame}, or an \code{edsurvey.data.frame.list}
#' @param groupA   an expression or character expression that defines a  condition for subset.
#'                 This subset will be compared to \code{groupB}. If not specified, it will define
#'                 a whole sample as in \code{data}. 
#' @param groupB   an expression or character expression that defines a  condition for subset.
#'                 This subset will be compared to \code{groupA}. If not specified, it will define
#'                 a whole sample as in \code{data}. If set to \code{NULL}, estimates for the second group 
#'                 will be dropped.
#' @param percentiles a numeric vector. The \code{gap} function calculates the
#'                    mean when this
#'                    argument is omitted or set to \code{NULL}. Otherwise,
#'                    the gap at the percentile given is calculated.
#' @param targetLevel a character string. When specified, calculates the gap in
#'                    the percentage of students at
#'                    \code{targetLevel} in \code{variable}. This is useful for
#'                    comparing the gap in the percentage of students at a
#'                    survey response level.
#' @param achievementLevel the achievement level(s) at which percentages
#'                         should be calculated
#' @param achievementDiscrete a logical indicating if the achievement level
#'                            specified in the \code{achievementLevel}
#'                            argument should be interpreted as discrete
#'                            so that
#'                            just the percentage in that particular achievement
#'                            level
#'                            will be included. Defaults to \code{FALSE}
#'                            so that
#'                            the percentage at or above that achievement level
#'                            will be
#'                            included in the percentage.
#' @param weightVar a character indicating the weight variable to use.
#'                  See Details.
#' @param jrrIMax   a numeric value; when using the jackknife variance estimation
#'                  method, the \eqn{V_{jrr}} term
#'                  (see Details) can be estimated with any positive number
#'                  of plausible values and is 
#'                  estimated on the lower of the number of
#'                  available plausible values and 
#'                  \code{jrrIMax}. When \code{jrrIMax} is set to \code{Inf},
#'                  all plausible values will 
#'                  be used. Higher values of \code{jrrIMax} lead to longer
#'                  computing times and more
#'                  accurate variance estimates.
#' @param varMethod  a character set to \code{jackknife} or \code{Taylor}
#'                   that indicates the variance estimation method
#'                   to be used
#' @param omittedLevels a logical value. When set to the default value of
#'                      \code{TRUE}, drops those levels of 
#'                      all factor variables.
#'                      Use \code{print} on an \code{edsurvey.data.frame}
#'                      to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value
#'                          of \code{TRUE}, uses the default 
#'                          conditions stored in \code{edsurvey.data.frame}
#'                          to subset the data. 
#'                          Use \code{print} on an \code{edsurvey.data.frame}
#'                          to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}.
#'               Can be set as
#'               \code{recode} \code{=} \code{list(var1} \code{=}
#'               \code{list(from} \code{=} \code{c("a",} \code{"b",}
#'               \code{"c"),} \code{to} \code{=} \code{"d"))}.
#'               See Examples.
#' @param referenceDataIndex a numeric used only when \code{data} is an
#'                           \code{edsurvey.data.frame.list},
#'                           indicating which dataset is the reference
#'                           dataset that other datasets are compared to. 
#'                           Defaults to one.
#' @param returnVarEstInputs a logical value; set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates. This is intended to allow for the
#'                           computation
#'                           of covariances between estimates.
#' @param returnSimpleDoF    a logical value set to \code{TRUE} to return the degrees
#'                           of freedom for some statistics (see Value
#'                           section) that do not have a
#'                           \emph{t}-test; useful primarily for further computation
#' @param returnSimpleN      a logical value set to \code{TRUE} to add the count
#'                           (\emph{n}-size) of observations included in groups A and B
#'                           in the percentage object
#'                         
#' @details This function calculates the gap between \code{groupA} and \code{groupB} (which 
#' may be omitted to indicate the full sample). The gap is
#' calculated for one of four statistics:
#' \describe{
#'   \item{the gap in means}{The mean score gap (in the score
#'      variable) identified in the \code{variable} argument.
#'      This is the default. The means and their standard errors are
#'      calculated using the methods
#'      described in the \code{\link{lm.sdf}} function documentation.}
#'   \item{the gap in percentiles}{The gap between respondents at
#'      the percentiles specified in the \code{percentiles} argument.
#'      This is returned when the \code{percentiles} argument is
#'      defined. The mean and standard error are computed as described in the 
#'      \code{\link{percentile}} function documentation.}
#'   \item{the gap in achievement levels}{The gap in the percentage of 
#'      students at (when \code{achievementDiscrete} is \code{TRUE}) or at
#'      or above (when \code{achievementDiscrete} is \code{FALSE}) a
#'      particular achievement level. This is used when the 
#'      \code{achievementLevel} argument is defined. The mean and standard error
#'      are calculated as described in the \code{\link{achievementLevels}}
#'      function documentation.}
#'   \item{the gap in a survey response}{The gap in the percentage of
#'      respondents responding at \code{targetLevel} to 
#'      \code{variable}. This is used when \code{targetLevel} is
#'      defined. The mean and standard deviation are calculated as described in
#'      the \code{\link{edsurveyTable}} function documentation.}
#' }
#' 
#' @return
#' The return type depends on if the class of the \code{data} argument is an
#' \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}. Both
#' include the call (called \code{call}), a list called \code{labels} that
#' shows the definition of
#' \code{groupA} and \code{groupB}, an object named \code{percentage}
#' that shows the percentage in \code{groupA} and \code{groupB}, and an object
#' that shows the gap called \code{results}. 
#'
#' The percentages are computed according to the
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistics vignette}
#'  section
#' \dQuote{Estimation of Weighted Percentages When Plausible Values Are Not Present.}
#' The standard errors are calculated according to
#' \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Not Present, Using the Jackknife Method.}
#' Standard errors of differences are calculated as the square root of the typical
#' variance formula
#' \deqn{Var(A-B) = Var(A) + Var(B) - 2 Cov(A,B)}
#' where the covariance term is calculated as described in the 
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistics vignette}
#'  section
#' \dQuote{Estimation of Covariances.} These degrees of freedom are available only
#' with the jackknife variance estimation. The degrees of freedom used for hypothesis testing
#' are always set to the number of jackknife replicates in the data.
#'
#' \subsection{the data argument is an edsurvey.data.frame}{
#'   When the \code{data} argument is an \code{edsurvey.data.frame},
#'   \code{gap} returns an S3 object of class \code{gap}. 
#' 
#'   The \code{percentage} object is a numeric vector with the following elements:
#'   \describe{
#'     \item{pctA}{the percentage of respondents in \code{groupA} compared with the whole sample in \code{data}}
#'     \item{pctAse}{the standard error on the percentage of respondents in
#'                       \code{groupA}}
#'     \item{dofA}{degrees of freedom appropriate for a \emph{t}-test involving \code{pctA}.
#'                 This value is returned only if 
#'                 \code{returnSimpleDoF}\code{=}\code{TRUE}}
#'     \item{pctB}{the percentage of respondents in \code{groupB}}
#'     \item{pctBse}{the standard error on the percentage of respondents in
#'                       \code{groupB}}
#'     \item{dofB}{degrees of freedom appropriate for a \emph{t}-test involving \code{pctA}.
#'                 This value is returned only if 
#'                 \code{returnSimpleDoF}\code{=}\code{TRUE}}
#'     \item{diffAB}{the value of \code{pctA} minus \code{pctB}}
#'     \item{covAB}{the covariance of \code{pctA} and \code{pctB}; used in
#'                  calculating \code{diffABse}}
#'     \item{diffABse}{the standard error of \code{pctA}
#'                            minus \code{pctB}}
#'     \item{diffABpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffAB}
#'                         is zero.}
#'     \item{dofAB}{degrees of freedom used in calculating
#'                       \code{diffABpValue}}
#'   }
#' 
#'   The \code{results} object is a numeric data frame with the following elements:
#'   \describe{
#'     \item{estimateA}{the mean estimate of \code{groupA} (or the percentage estimate
#'                      if \code{achievementLevel} or \code{targetLevel} is specified)}
#'     \item{estimateAse}{the standard error of \code{estimateA}}
#'     \item{dofA}{degrees of freedom appropriate for a \emph{t}-test involving \code{meanA}.
#'                 This value is returned only if 
#'                 \code{returnSimpleDoF}\code{=}\code{TRUE}.}
#'     \item{estimateB}{the mean estimate of \code{groupB} (or the percentage estimate
#'                      if \code{achievementLevel} or \code{targetLevel} is specified)}
#'     \item{estimateBse}{the standard error of \code{estimateB}}
#'     \item{dofB}{degrees of freedom appropriate for a \emph{t}-test involving \code{meanB}.
#'                 This value is returned only if 
#'                 \code{returnSimpleDoF}\code{=}\code{TRUE}.}
#'     \item{diffAB}{the value of \code{estimateA} minus \code{estimateB}}
#'     \item{covAB}{the covariance of \code{estimateA} and \code{estimateB}. Used in
#'                  calculating \code{diffABse}}
#'     \item{diffABse}{the standard error of \code{diffAB}}
#'     \item{diffABpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffAB}
#'                         is zero.}
#'     \item{dofAB}{degrees of freedom used for the \emph{t}-test on \code{diffAB}}
#'   }
#'   
#'   If the gap was in  achievement levels or percentiles and more
#'   than one percentile or achievement level is requested,
#'   then an additional column
#'   labeled \code{percentiles} or \code{achievementLevel} is included
#'   in the \code{results} object.
#'
#'   When \code{results} has a single row and when \code{returnVarEstInputs}
#'   is \code{TRUE}, the additional elements \code{varEstInputs} and
#'   \code{pctVarEstInputs} also are returned. These can be used for calculating
#'   covariances with \code{\link{varEstToCov}}.
#' }
#' 
#' \subsection{the data argument is an edsurvey.data.frame.list}{
#'   When the \code{data} argument is an \code{edsurvey.data.frame.list},
#'   \code{gap} returns an S3 object of class \code{gapList}.
#'   
#'   The \code{results} object in the \code{edsurveyResultList} is
#'   a \code{data.frame}. Each row regards a particular dataset from the
#'   \code{edsurvey.data.frame}, and a reference dataset is dictated by
#'   the \code{referenceDataIndex} argument.
#'   
#'   The \code{percentage} object is a \code{data.frame} with the following elements:
#'   \describe{
#'     \item{covs}{a data frame with a column for each column in the \code{covs}. See previous
#'                 section for more details.}
#'     \item{...}{all elements in the \code{percentage} object in the
#'                previous section}
#'     \item{diffAA}{the difference in \code{pctA} between the reference data
#'                   and this dataset. Set to \code{NA} for the
#'                   reference dataset.}
#'     \item{covAA}{the covariance of \code{pctA} in the reference data and
#'                  \code{pctA} on this row. Used in
#'                  calculating \code{diffAAse}.}
#'     \item{diffAAse}{the standard error for \code{diffAA}}
#'     \item{diffAApValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffAA}
#'                         is zero.}
#'     \item{diffBB}{the difference in \code{pctB} between the reference data
#'                   and this dataset. Set to \code{NA} for the
#'                   reference dataset.}
#'     \item{covBB}{the covariance of \code{pctB} in the reference data and
#'                  \code{pctB} on this row. Used in
#'                  calculating \code{diffAAse}.}
#'     \item{diffBBse}{the standard error for \code{diffBB}}
#'     \item{diffBBpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffBB}
#'                         is zero.}
#'     \item{diffABAB}{the value of \code{diffAB} in the reference dataset
#'                            minus the value of \code{diffAB} in this dataset. Set
#'                            to \code{NA} for the reference dataset.}
#'     \item{covABAB}{the covariance of \code{diffAB} in the reference data and
#'                    \code{diffAB} on this row. Used in
#'                    calculating \code{diffABABse}.}
#'     \item{diffABABse}{the standard error for \code{diffABAB}}
#'     \item{diffABABpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffABAB}
#'                         is zero.}
#'   }
#'
#'   The \code{results} object is a \code{data.frame} with the following elements:
#'   \describe{
#'     \item{...}{all elements in the \code{results} object in the
#'                previous section}
#'     \item{diffAA}{the value of \code{groupA} in the reference dataset minus
#'                          the value in this dataset. Set to \code{NA} for the
#'                          reference dataset}
#'     \item{covAA}{the covariance of \code{meanA} in the reference data and
#'                  \code{meanA} on this row. Used in
#'                  calculating \code{diffAAse}.}
#'     \item{diffAAse}{the standard error for \code{diffAA}.}
#'     \item{diffAApValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffAA}
#'                         is zero.}
#'     \item{diffBB}{the value of \code{groupB} in the reference dataset minus
#'                          the value in this dataset. Set to \code{NA} for the
#'                          reference dataset.}
#'     \item{covBB}{the covariance of \code{meanB} in the reference data and
#'                  \code{meanB} on this row. Used in
#'                  calculating \code{diffBBse}.}
#'     \item{diffBBse}{the standard error for \code{diffBB}}
#'     \item{diffBBpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffBB}
#'                         is zero.}
#'     \item{diffABAB}{the value of \code{diffAB} in the reference dataset
#'                            minus the value of \code{diffAB}
#'                            in this dataset. Set
#'                            to \code{NA} for the reference dataset.}
#'     \item{covABAB}{the covariance of \code{diffAB} in the reference data and
#'                    \code{diffAB} on this row. Used in
#'                    calculating \code{diffABABse}.}
#'     \item{diffABABse}{the standard error for \code{diffABAB}}
#'     \item{diffABABpValue}{the \emph{p}-value associated with the \emph{t}-test used
#'                         for the hypothesis test that \code{diffABAB}
#'                         is zero.}
#'     \item{sameSurvey}{a logical value indicating if this line uses the same
#'                       survey as the reference line. Set to \code{NA} for the
#'                       reference line.}
#'   }
#'
#' }
#'
#' @author Paul Bailey and Trang Nguyen
#' @importFrom stats formula
#' @example man/examples/gap.R
#' @export
gap <- function(variable, data, groupA = "default", groupB = "default",
                percentiles=NULL,
                achievementLevel=NULL,
                achievementDiscrete=FALSE,
                targetLevel=NULL,
                weightVar=NULL, jrrIMax=1,
                varMethod=c("jackknife", "Taylor"),
                omittedLevels=TRUE,
                defaultConditions=TRUE,
                recode=NULL,
                referenceDataIndex=1,
                returnVarEstInputs=FALSE,
                returnSimpleDoF=FALSE,
                returnSimpleN=FALSE) {
  if(is.character(substitute(groupA))) {
    groupA <- parse(text=groupA)[[1]]
  }
  if (all(as.character(substitute(groupA)) == 'default')) {
    groupB <- NULL
  }
  if(!missing(groupB)) {
    if(is.character(substitute(groupB))) {
      groupB <- parse(text=groupB)[[1]]
    }
  }
  
  # check incoming variables
  call <- match.call()
  varMethod <- substr(tolower(varMethod[[1]]), 0,1)
 
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  # if recode list is provided, it will be added to userConditions at first (before subset in groupA and groupB)
  if (!is.null(recode)) {
    data <- recode.sdf(data, recode)
  }
  # deal with the possibility that data is an edsurvey.data.frame.list
  if(inherits(data, "edsurvey.data.frame.list") | length(percentiles) > 1 | length(achievementLevel) > 1) {
    # a flag that is TRUE when there is no information in data$covs. Default to false
    nocovs <- FALSE
    if(!inherits(data, "edsurvey.data.frame.list")) {
      # this is a situation where data is not an esdfl. The following code
      # required it to be one, so make it one. But note that covs  
      # does not have real information in it.
      nocovs <- TRUE
      data <- edsurvey.data.frame.list(list(data), labels="data")
    }
    ll <- length(data$datalist)
    lpct <- ifelse(is.null(percentiles), 1, length(percentiles))
    lal <- ifelse(is.null(achievementLevel), 1, length(achievementLevel))
    
    # check variable specific to edsurvey.data.frame.list
    suppressWarnings(refi <- as.integer(referenceDataIndex))
    if(!refi %in% 1:ll) {
      stop(paste0("The argument, ", sQuote("referenceDataIndex"), " must be an integer between 1 and ", ll, "."))
    }
    res <- list(results=list(),
                labels=list(A=substitute(groupA),B=substitute(groupB)))
    
    pctdf0 <- data$covs
    if(!nocovs) {
      pctdf0$covAA <- 0
      pctdf0$dofAA <- Inf
      pctdf0$covBB <- 0
      pctdf0$dofBB <- Inf
      pctdf0$covABAB <- 0
      pctdf0$dofABAB <- Inf
    }
    
    
    # make sure refi gets calculated first
    llvec <- unique(c(refi,1:ll))
    resilist <- list()
    # call gap for each edsurvey.data.frame
    for(i in llvec) {
      # for each element of the edsurvey.data.frame.list
      calli <- call
      calli$varMethod <- varMethod
      # adjust the call per the needs of this call
      calli$data <- data$datalist[[i]]
      # we need the these inputs to calculate across essurvey.data.frame
      # covariances (but, only if they are the same dataset) an issue
      # that will be resolved later.
      calli$returnVarEstInputs <- TRUE
      # this returns the degrees of freedom for meanA and meanB
      # used for diffAA and diffBB, respectively
      calli$returnSimpleDoF <- TRUE
      # if these are NULL then setting them changes nothing
      calli$percentiles <- percentiles
      calli$achievementLevel <- achievementLevel
      calli <- as.list(calli)
      # use the same list of arguments but change the function to gapHelper
      calli[[1]] <- as.name("gapHelper")
      resilist[[i]] <- tryCatch(resi <- eval(as.call(calli)),
                                error=function(cond) {
                                  message(paste0("An error occurred while working on a dataset ",
                                                 paste(dQuote(data$covs[i,]), collapse=", "),
                                                 ". The results from that dataset will be excluded. Error message: ",
                                                 cond))
                                  return(0)
                                })
      # if there was not an error
      if(!inherits(resilist[[i]], "gap")) {
        # there was an error
        if(i %in% referenceDataIndex) {
          stop("An error prevented processing of the reference dataset. This must be fixed before comparisons to the reference dataset can be made.")
        }
      } else {
        resi$call <- NULL

        # the following code is to clean up the results (adding some columns to make the code to extract results from gapHelper more consistent)
        # after this code, we should expect that each gap result will have:
        # 1. resi$results will have 'statisticsLevel' column: indicate percentile or achievementLevel, or 'data' if none is specified
        # 2. resi$varEstInputs: PV and JK data frame will have 'Level' column similar to 'statisticsLevel' column in resi$results
        if (is.null(percentiles) && is.null(achievementLevel)) {
          resi$results$statisticsLevel <- "data"
        }
        # if no or only 1 percentile/achievementLevel is specified
        if (length(percentiles) <=1 && length(achievementLevel) <=1) {
          resi$varEstInputs$JK$Level <- "data"
          if (!is.null(resi$varEstInputs$PV)) { #PV is null for type = "eq"
            resi$varEstInputs$PV$Level <- "data"
          }
        }
        # in gapHelper, type = 'pct' will return 'percentiles' column and type = 'al' will return 'achievementLevel' column
        # this replacement is to use 1 consistent name for the result extraction code below
        # it will be renamed back to original names after the extraction
        colnames(resi$results) <- gsub("achievementLevel|percentiles","statisticsLevel",colnames(resi$results))
        resilist[[i]] <- resi
      }
    } # end for(i in llvec)
    
    
    # Extract results and compute AA or BB statistics
    lstats <- max(lpct,lal)
    for(j in 1:lstats) {
      #for(jal in 1:lal) {
        resdf <- data$covs
        if(!nocovs) {
          # only generate these columns if there are multiple datasets
          resdf$covAA <- 0
          resdf$dofAA <- Inf
          resdf$covBB <- 0
          resdf$dofBB <- Inf
          resdf$covABAB <- 0
          resdf$dofABAB <- Inf
        }
        # to check whether the two samples are from the same survey or not
        # we need to use DOFCorrection and compute covariance if the two samples are from the same survey
        resdf$sameSurvey <- ifelse(1:nrow(resdf) == refi,
                                   rep(NA, nrow(resdf)),
                                   rep(FALSE, nrow(resdf)))
        if(lpct > 1) {
          resdf$percentiles <- percentiles[[j]]
        }
        for (i in llvec) {
          if(!inherits(resilist[[i]], "gap")) {
            # there was an error
            if(i %in% referenceDataIndex) {
              stop("An error prevented processing of the reference dataset. This must be fixed before comparisons to the reference dataset can be made.")
            }
            # this is to make sure that all achievementLevels or percentiles are created as to the reference edsurvey.data.frame
            # Missing values will be NA for statistics
            if (!is.null(achievementLevel)) {
              resdf$achievementLevel[i] <- resilist[[refi]]$results$statisticsLevel[j]
            } else if (!is.null(percentiles)) {
              resdf$percentiles[i] <- resilist[[refi]]$results$statisticsLevel[j]
            }
          } else { # there was not an error
            # dataframes do not like vectorwise assignment by column name,
            # so assign one at a time
            resi <- resilist[[i]]
    
            resi$results <- resi$results[j,]
            resi$varEstInputs$JK=resi$varEstInputs$JK[tolower(resi$varEstInputs$JK$Level) %in% tolower(c(resi$results$statisticsLevel,"data")),]
            if (!is.null(resi$varEstInputs$PV)) {
              resi$varEstInputs$PV <- resi$varEstInputs$PV[tolower(resi$varEstInputs$PV$Level) %in% tolower(c(resi$results$statisticsLevel,"data")),]
            }
                                                
            if (!is.null(achievementLevel)) {
              colnames(resi$results) <- gsub("statisticsLevel","achievementLevel",colnames(resi$results))
            } else if (!is.null(percentiles)) {
              colnames(resi$results) <- gsub("statisticsLevel","percentiles",colnames(resi$results))
            } else {
              resi$results$statisticsLevel <- NULL
            }
            for(ii in 1:length(resi$results)) {
              resdf[i,names(resi$results)[ii]] <- resi$results[[ii]]
            }
            skipB <- is.null(resi$labels$B) # a boolean to indicate whether groupB estimates are available in the results of gapHelper
            if(i == refi) {
              refVarEstInputs <- resi$varEstInputs
              refPctVarEstInputs <- resi$pctVarEstInputs
            }
            # refi is the index of reference dataset
            # AA and BB statistics of reference dataset will be set to NA
            if(i != refi) {
              resdf$dofAA[i] <- dofCompute((resi$results)$estimateAse, resdf[refi,"estimateAse"] ,(resi$results)[["dofA"]], resdf[refi,"dofA"])
              if (!skipB) {
                resdf$dofBB[i] <- dofCompute((resi$results)$estimateBse, resdf[refi,"estimateBse"] ,(resi$results)[["dofB"]], resdf[refi,"dofB"])
                resdf$dofABAB[i] <- dofCompute((resi$results)$diffABse, resdf[refi,"diffABse"] ,(resi$results)[["dofAB"]], resdf[refi,"dofAB"])
              }
              # if the datasets and from the same sample, add covariances and do DoFCorrection
              if(sameSurvey(data$datalist[[i]], data$datalist[[refi]])) {
                if (nrow(resi$varEstInputs$JK) != 0) { # some Level (percentile or achievementLevel) do not have varEstInput data so the subset
                									   # in line 476 will return an empty data.frame
                  resdf$dofAA[i] <- DoFCorrection(resi$varEstInputs, refVarEstInputs, "A", method="JR")
                }
                resdf$covAA[i] <- varEstToCov(resi$varEstInputs, refVarEstInputs, "A", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                if (!skipB & nrow(resi$varEstInputs$JK) != 0) {
                  resdf$dofBB[i] <- DoFCorrection(resi$varEstInputs, refVarEstInputs, "B", method="JR")
                  resdf$dofABAB[i] <- DoFCorrection(resi$varEstInputs, refVarEstInputs, "A-B", method="JR")
                  resdf$covBB[i] <- varEstToCov(resi$varEstInputs, refVarEstInputs, "B", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                  resdf$covABAB[i] <- varEstToCov(resi$varEstInputs, refVarEstInputs, "A-B", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                }
                resdf$sameSurvey[i] <- TRUE
              }
            } else { # end if(i != refi)
              # if this is refi, set these to NULL
              resdf$covAA[i] <- resdf$covBB[i] <- resdf$covABAB[i] <- NA
              resdf$dofAA[i] <- resdf$dofBB[i] <- resdf$dofABAB[i] <- NA
            }
            # for the first j value only, grab the percent
            if(j == 1) {
              for(ii in 1:ncol(resi$percentage)) {
                pctdf0[i,colnames(resi$percentage)[ii]] <- resi$percentage[,ii] 
                if(i != refi) {
                  pctdf0$dofAA[i] <- dofCompute((resi$percentage)$pctAse, pctdf0[refi,"pctAse"] ,(resi$percentage)[["dofA"]], pctdf0[refi,"dofA"])
                  if (!skipB) {
                    pctdf0$dofBB[i] <- dofCompute((resi$percentage)$pctBse, pctdf0[refi,"pctBse"] ,(resi$percentage)[["dofB"]], pctdf0[refi,"dofB"])
                    pctdf0$dofABAB[i] <- dofCompute((resi$percentage)$diffABse, pctdf0[refi,"diffABse"] ,(resi$percentage)[["dofAB"]], pctdf0[refi,"dofAB"])
                  }
                  
                  if(sameSurvey(data$datalist[[i]], data$datalist[[refi]])) {
                    pctdf0$dofAA[i] <- DoFCorrection(resi$pctVarEstInputs, refPctVarEstInputs, "A", method="JR")
                    pctdf0$covAA[i] <- varEstToCov(resi$pctVarEstInputs, refPctVarEstInputs, "A", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                    if (!skipB) {
                      pctdf0$dofBB[i] <- DoFCorrection(resi$pctVarEstInputs, refPctVarEstInputs, "B", method="JR")
                      pctdf0$dofABAB[i] <- DoFCorrection(resi$pctVarEstInputs, refPctVarEstInputs, "A-B", method="JR")
                      pctdf0$covBB[i] <- varEstToCov(resi$pctVarEstInputs, refPctVarEstInputs, "B", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                      pctdf0$covABAB[i] <- varEstToCov(resi$pctVarEstInputs, refPctVarEstInputs, "A-B", jkSumMultiplier=getAttributes(data$datalist[[i]], "jkSumMultiplier"))
                    }
                  }
                } else { # end if(i != refi)
                  # if this is refi, set these to NULL
                  pctdf0$covAA[i] <- pctdf0$covBB[i] <- pctdf0$covABAB[i] <- NA
                  pctdf0$dofAA[i] <- pctdf0$dofBB[i] <- pctdf0$dofABAB[i] <- NA
                } # end else for (i != refi)
              } # end for(ii in 1:ncol(resi$percentage))
              if(returnSimpleN) {
                # return the n counts
                pctdf0$nA[i] <- resi$labels$nUsedA
                pctdf0$nB[i] <- resi$labels$nUsedB
              }
            } # end if(j == 1) 
          } # end if(class(temp) == "gap")
        } # End of for(i in 1:i) loop
        # Compute diff statistics (across different edsurvey.data.frame)
        resdf$diffAA <- ifelse(1:nrow(resdf) == refi, NA, resdf$estimateA[refi] - resdf$estimateA)
        resdf$diffAAse <- ifelse(1:nrow(resdf) == refi, NA, sqrt(resdf$estimateAse[refi]^2 + resdf$estimateAse^2 - 2 * resdf$covAA))
        resdf$diffAApValue <- ifelse(1:nrow(resdf) == refi, NA, 2*(1-pt2(abs(resdf$diffAA/resdf$diffAAse), df=resdf$dofAA)))
        if (!skipB) {
          resdf$diffBB <- ifelse(1:nrow(resdf) == refi, NA, resdf$estimateB[refi] - resdf$estimateB)
          resdf$diffBBse <- ifelse(1:nrow(resdf) == refi, NA, sqrt(resdf$estimateBse[refi]^2 + resdf$estimateBse^2 - 2 * resdf$covBB))
          resdf$diffBBpValue <- ifelse(1:nrow(resdf) == refi, NA, 2*(1-pt2(abs(resdf$diffBB/resdf$diffBBse), df=resdf$dofBB)))
          resdf$diffABAB <- ifelse(1:nrow(resdf) == refi, NA, resdf$diffAB[refi] - resdf$diffAB)
          resdf$diffABABse <- ifelse(1:nrow(resdf) == refi, NA, sqrt(resdf$diffABse[refi]^2 + resdf$diffABse^2 - 2 * resdf$covABAB))
          resdf$diffABABpValue <- ifelse(1:nrow(resdf) == refi, NA, 2*(1-pt2(abs(resdf$diffABAB/resdf$diffABABse), df=resdf$dofABAB)))
        }
        
        # only generate these 
        if(!nocovs) {
          pctdf0$diffAA <- ifelse(1:nrow(pctdf0) == refi, NA, pctdf0$pctA[refi] - pctdf0$pctA)
          pctdf0$diffAAse <- ifelse(1:nrow(pctdf0) == refi, NA, sqrt(pctdf0$pctAse[refi]^2 + pctdf0$pctAse^2 - 2 * pctdf0$covAA))
          pctdf0$diffAApValue <- ifelse(1:nrow(pctdf0) == refi, NA, 2*(1-pt2(abs(pctdf0$diffAA/pctdf0$diffAAse), df=pctdf0$dofAA)))
          if (!skipB) {
            pctdf0$diffBB <- ifelse(1:nrow(pctdf0) == refi, NA, pctdf0$pctB[refi] - pctdf0$pctB)
            pctdf0$diffBBse <- ifelse(1:nrow(pctdf0) == refi, NA, sqrt(pctdf0$pctBse[refi]^2 + pctdf0$pctBse^2 - 2 * pctdf0$covBB))
            pctdf0$diffBBpValue <- ifelse(1:nrow(pctdf0) == refi, NA, 2*(1-pt2(abs(pctdf0$diffBB/pctdf0$diffBBse), df=pctdf0$dofBB)))
            pctdf0$diffABAB <- ifelse(1:nrow(pctdf0) == refi, NA, pctdf0$diffAB[refi] - pctdf0$diffAB)
            pctdf0$diffABABse <- ifelse(1:nrow(pctdf0) == refi, NA, sqrt(pctdf0$diffABse[refi]^2 + pctdf0$diffABse^2 - 2 * pctdf0$covABAB))
            pctdf0$diffABABpValue <- ifelse(1:nrow(pctdf0) == refi, NA, 2*(1-pt2(abs(pctdf0$diffABAB/pctdf0$diffABABse), df=pctdf0$dofABAB)))
          }
        }
        if(j==1) {
          resdf0 <- resdf
        } else {
          resdf0 <- rbind(resdf0, resdf)
        }
    } # end for(j in 1:lstats) loop
    if(nocovs) {
      # remove the labels columns which are just populated
      # with the unhelpful text "data"
      pctdf0$labels <- NULL
      resdf0$labels <- NULL
      # reorder columns when there are no covariates
      if(returnSimpleDoF) {
        if (!skipB) {
          resvar0 <- c("estimateA", "estimateAse", "dofA",
                       "estimateB", "estimateBse", "dofB")
        } else {
          resvar0 <- c("estimateA", "estimateAse", "dofA")
        }
        
      } else {
        if (!skipB) {
          resvar0 <- c("estimateA", "estimateAse",
                       "estimateB", "estimateBse")
        } else {
          resvar0 <- c("estimateA", "estimateAse")
        }
        
      }
      if (!skipB) {
        resvar0 <- c(resvar0,
                     "diffAB", "covAB",
                     "diffABse", "diffABpValue", "dofAB")
      }
      addons <- c("achievementLevel", "percentiles")
      resvar <- c(addons[addons %in% names(resdf0)], resvar0)
      if("sameSurvey" %in% names(resdf0) && sum(!is.na(resdf0$sameSurvey)) > 0) {
        resvar <- c(resvar, "sameSurvey")
      }
      resdf0 <- resdf0[,resvar]
      # remove "estimate" with "pct" where "estimate" must start the variable name
      resvar <- gsub("^estimate", "pct", resvar0)
      if(returnSimpleN) {
        if (!skipB) {
          resvar <- c(resvar, "nA", "nB")
        } else {
          resvar <- c(resvar, "nA")
        }
        
      }
      pctdf0 <- pctdf0[,resvar]
    } else { # end  if(nocovs)
      # reorder columns when there are covs
      if(returnSimpleDoF) {
        if (!skipB) {
          resvar0 <- c(names(data$covs), "estimateA", "estimateAse", "dofA",
                       "estimateB", "estimateBse", "dofB")
        } else {
          resvar0 <- c(names(data$covs), "estimateA", "estimateAse", "dofA")
        }
        
      } else {
        if (!skipB) {
          resvar0 <- c(names(data$covs), "estimateA", "estimateAse",
                       "estimateB", "estimateBse")
        } else {
          resvar0 <- c(names(data$covs), "estimateA", "estimateAse")
        }
        
      }
      if (!skipB) {
        resvar0 <- c(resvar0,
                     "diffAB", "covAB", "diffABse",
                     "diffABpValue", "dofAB",
                     "diffAA", "covAA", "diffAAse",
                     "diffAApValue", "dofAA",
                     "diffBB", "covBB", "diffBBse",
                     "diffBBpValue", "dofBB",
                     "diffABAB", "covABAB", "diffABABse",
                     "diffABABpValue", "dofABAB")
      } else {
        resvar0 <- c(resvar0,
                     "diffAA", "covAA", "diffAAse",
                     "diffAApValue", "dofAA")
      }
      
      addons <- c("achievementLevel", "percentiles")
      resvar <- c(addons[addons %in% names(resdf0)], resvar0)
      if("sameSurvey" %in% names(resdf0) && sum(!is.na(resdf0$sameSurvey)) > 0) {
        resvar <- c(resvar, "sameSurvey")
      }
      resdf0 <- resdf0[,resvar]
      resvar <- gsub("^estimate", "pct", resvar0)
      missing <- resvar[!resvar %in% names(pctdf0)] 
      if(returnSimpleN) {
        if (!skipB) { resvar <- c(resvar, "nA", "nB") } else {resvar <- c(resvar,"nA")}
      }
      pctdf0 <- pctdf0[,resvar]
    } # end else  if(nocovs)
    res[["results"]] <- resdf0
    res[["percentage"]] <- pctdf0
    res[["call"]] <- call
    class(res) <- "gapList"
    return(res)        
    
    
  } else { # end if(inherits(data, "edsurvey.data.frame.list"))
    calli <- as.list(call)
    calli[[1]] <- as.name("gapHelper")
    calli$varMethod <- varMethod
    calli$data <- data
    resi <- eval(as.call(calli))
    resi$call <- call
    return(resi)
  } # end else for if(inherits(data, "edsurvey.data.frame.list")) {
}


# this is called when data is edsurvey.data.frame or light.edsurvey.data.frame
gapHelper <- function(variable, data, groupA = "default", groupB = "default",
                      percentiles=NULL,
                      achievementLevel=NULL,
                      achievementDiscrete=FALSE,
                      targetLevel=NULL,
                      weightVar=NULL, jrrIMax=1,
                      varMethod=c("jackknife", "Taylor"),
                      omittedLevels=TRUE,
                      defaultConditions=TRUE,
                      recode=NULL,
                      referenceDataIndex=1,
                      returnVarEstInputs=FALSE,
                      returnSimpleDoF=FALSE,
                      returnSimpleN=FALSE) {
  if(is.character(substitute(groupA))) {
    groupA <- parse(text=groupA)[[1]]
  }
  if (all(as.character(substitute(groupA)) == 'default')) {
    groupB <- NULL
  }
  if(!missing(groupB)) {
    if(is.character(substitute(groupB))) {
      groupB <- parse(text=groupB)[[1]]
    }
  }
 
  # get the weight var
  # if the weight var is not set, use the default
  if(is.null(weightVar)) {
    wgt <- attributes(getAttributes(data, "weights"))$default
  } else {
    wgt <- weightVar
  } # End of if/else: is.null(weightVar)
  
  varEstInputs <- NULL
  type <- "mu" # mean is the default
  if(!is.null(percentiles)) {
    type <- "pct" 
  }
  if(!is.null(achievementLevel)) {
    if(type != "mu") {
      stop(paste0("Only one of ", sQuote("percentiles"), ", ", sQuote("achievementLevel"), ", and ", sQuote("targetLevel"), " can be defined."))
    }
    type <- "AL"
    if (any(!tolower(gsub("(below |at or above )","",achievementLevel, ignore.case = TRUE)) %in% tolower(names(getAttributes(data,"achievementLevels"))))) {
      stop("Achievement Level ",sQuote(achievementLevel), " is not available for the current survey.")
    }
  }
  if(!is.null(targetLevel)) {
    if(type != "mu") {
      stop(paste0("Only one of ", sQuote("percentiles"), ", ", sQuote("achievementLevel"), ", and ", sQuote("targetLevel"), " can be defined."))
    }
    type <- "eq"
  }
  
  # make the data with just groupA
  if (all(as.character(substitute(groupA)) == "default")) {
    dataA <- data
  } else {
    dataA <- subset(data, substitute(groupA), inside=TRUE)
  }
  # skipB is a boolean to indicate whether we need to compute statistics for group B
  # skipB = TRUE happens when (1) groupB is set to NULL or (2) groupA = 'default' (full sample)
  skipB <- FALSE
  # if groupB is null, just use data as the reference.
  # NOTE: the call to is.null requires "substitute" or groupB
  # will be evaluated right here
  if (is.null(substitute(groupB))) {
    skipB <- TRUE
  } else if(all(as.character(substitute(groupB)) == "default")) {
    dataB <- data
    dataAB <- dataA
  } else {
    # otherwise filter on groupB
    dataB <- subset(data, substitute(groupB), inside=TRUE)
    dataAB <- subset(dataA, substitute(groupB), inside=TRUE)
  }
  callv <- list(weightVar=weightVar,
                jrrIMax=jrrIMax,
                omittedLevels=omittedLevels,
                recode=NULL) # recode.sdf is already done in line 335
  if(!missing(defaultConditions)) {
    callv <- c(callv, list(defaultConditions=defaultConditions))
  }
  if(type == "mu") {
    # gap in mean
    calllm <- c(list(formula(paste0(variable, " ~ 1")),varMethod=varMethod),
                callv,
                list(returnVarEstInputs=TRUE)) # necessary for covariance est
    calllmA <- c(calllm, list(data=dataA))
    meanA <- do.call(lm.sdf, calllmA)
    coefA <- meanA$coefmat
    resA <- coefA[1,1]
    if (!skipB) {
      calllmB <- c(calllm, list(data=dataB))
      meanB <- do.call(lm.sdf, calllmB)
      diff <- unname(meanA$coef[1] - meanB$coef[1]) # unname removes the name (Intercept) that otherwise ends up in the retruned value
      coefB <- meanB$coefmat
      resB <- coefB[1,1]
      SEs <- list(estimateAse=coefA[1,2], estimateBse=coefB[1,2])
    } else {
      SEs <- list(estimateAse=coefA[1,2])
    }
    
    
    # prepare varEstInputs
    # make varEstInputs$JK
    maJK <- subset(meanA$varEstInputs$JK, variable=="(Intercept)")
    maJK$variable <- "A"
    if (!skipB) {
      mbJK <- subset(meanB$varEstInputs$JK, variable=="(Intercept)")
      mbJK$variable <- "B"
      # calculate the difference
      mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate"), suffixes=c(".A", ".B"))
      mdJK$variable <- "A-B"
      mdJK$value <- mdJK$value.A - mdJK$value.B
      mdJK$value.A <- mdJK$value.B <- NULL
      mdJK$variable.A <- mdJK$variable.B <- NULL
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
    
    # make varEstInputs$PV
    if(!is.null(meanA$varEstInput$PV)) {
      maPV <- subset(meanA$varEstInputs$PV, variable=="(Intercept)")
      maPV$variable <- "A"
      if (!skipB) {
        mbPV <- subset(meanB$varEstInputs$PV, variable=="(Intercept)")
        mbPV$variable <- "B"
        # calculate the difference
        mdPV <- merge(maPV, mbPV, by=c("PV"), suffixes=c(".A", ".B"))
        mdPV$variable <- "A-B"
        mdPV$value <- mdPV$value.A - mdPV$value.B
        mdPV$value.A <- mdPV$value.B <- NULL
        mdPV$variable.A <- mdPV$variable.B <- NULL
        varEstPV <- rbind(maPV, mbPV, mdPV)
      } else {
        varEstPV <- maPV
      }
    } else {
      varEstPV <- NULL
    }
    # make varEstInputs
    row.names(varEstJK) <- NULL
    row.names(varEstPV) <- NULL
    varEstInputs <- list(JK=varEstJK, PV=varEstPV)
  }
  if(type == "pct") {
    # gap at a percentile
    # here the length of percentiles is always one
    callpct <- c(list(variable=variable,
                      percentiles=percentiles,
                      varMethod=varMethod),
                 callv,
                 returnVarEstInputs=TRUE)
    callpctA <- c(callpct, list(data=dataA))
    meanA <- do.call(percentile, callpctA)
    resA <- meanA$estimate
    statisticsLevelOut <- paste0("P",percentiles)
    if (!skipB) {
      callpctB <- c(callpct, list(data=dataB))
      meanB <- do.call(percentile, callpctB)
      resB <- meanB$estimate
      SEs <- list(estimateAse=meanA$se, estimateBse=meanB$se)
    } else {
      SEs <- list(estimateAse = meanA$se)
    }
    
    maJK <- attributes(meanA)$varEstInputs$JK
    maJK$Level <- maJK$variable
    maJK$variable <- "A"
    if (!skipB) {
      mbJK <- attributes(meanB)$varEstInputs$JK
      mbJK$Level <- mbJK$variable
      mbJK$variable <- "B"
      # calculate the difference
      mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate","Level"), suffixes=c(".A", ".B"))
      mdJK$variable <- "A-B"
      mdJK$value <- mdJK$value.A - mdJK$value.B
      mdJK$value.A <- mdJK$value.B <- NULL
      mdJK$variable.A <- mdJK$variable.B <- NULL
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
    
    # make varEstInputs$PV
    maPV <- attributes(meanA)$varEstInputs$PV
    maPV$Level <- maPV$variable
    maPV$variable <- "A"
    
    if (!skipB) {
      mbPV <- attributes(meanB)$varEstInputs$PV
      mbPV$Level <- mbPV$variable
      mbPV$variable <- "B"
      # calculate the difference
      mdPV <- merge(maPV, mbPV, by=c("PV","Level"), suffixes=c(".A", ".B"))
      mdPV$variable <- "A-B"
      mdPV$value <- mdPV$value.A - mdPV$value.B
      mdPV$value.A <- mdPV$value.B <- NULL
      mdPV$variable.A <- mdPV$variable.B <- NULL
      varEstPV <- rbind(maPV, mbPV, mdPV)
    } else {
      varEstPV <- maPV
    }
    
    # make varEstInputs
    row.names(varEstJK) <- NULL
    row.names(varEstPV) <- NULL
    varEstInputs <- list(JK=varEstJK, PV=varEstPV)
  } # end if (type == 'pct')
  if(type == "AL") {
    # gap in percent at or above an achievement level
    callal <- c(list(achievementVars=variable,
                     returnVarEstInputs=TRUE,
                     returnCumulative = !achievementDiscrete),
                callv)
    # if(!achievementDiscrete) { 
    #   # the call will return a discrete, but the only level will be the one in question
    #   # this will make it cumulative.
    #   dataA$pvvars[[variable]]$achievementLevel <- getAttributes(dataA,"pvvars")[[variable]]$achievementLevel[achievementLevel]
    #   if (!skipB) {
    #     dataB$pvvars[[variable]]$achievementLevel <- getAttributes(dataB,"pvvars")[[variable]]$achievementLevel[achievementLevel]
    #   }
    # }
    als <- getAttributes(data,"achievementLevels") # get a full achievementLevels
    callalA <- c(callal, list(data=dataA))
    meanA <- do.call(achievementLevels, callalA)
    if (!skipB) {
      callalB <- c(callal, list(data=dataB))
      meanB <- do.call(achievementLevels, callalB)
    }
    # achievementDiscrete indicates whether we should extract cumulative or discrete achievement level results
    if (achievementDiscrete){
      # get index of desired achievement levels from the results
      lA <- sapply(achievementLevel, function(al) {
        if (grepl("below", al, ignore.case = TRUE)) {
          lal <- al
        } else {
          lal <- paste0("At ",al)
        }
        grep(lal,meanA$discrete$Level, ignore.case = TRUE)
      })
      if (length(lA) == 0) {
        stop("Achievement Level cannot be found in the results of the call of the function ",sQuote("achievementLevel"))
      }
      if (!skipB) {
        lB <- sapply(achievementLevel, function(al) {
          if (grepl("below", al, ignore.case = TRUE)) {
            lal <- al
          } else {
            lal <- paste0("At ",al)
          }
          grep(lal,meanA$discrete$Level, ignore.case = TRUE)
        })
        if (length(lB) == 0) {
          stop("Achievement Level cannot be found in the results of the call of the function ",sQuote("achievementLevel"))
        }
      }
      resA <- meanA$discrete$Percent[lA]
      if (!skipB) {
        resB <- meanB$discrete$Percent[lB]
        SEs <- list(estimateAse=meanA$discrete$StandardError[lA],
                    estimateBse=meanB$discrete$StandardError[lB])
      } else {
        SEs <- list(estimateAse=meanA$discrete$StandardError[lA])
      }
      
    } else {
      # get index of desired achievement levels from the results
      lA <- sapply(achievementLevel, function(al) {
        if (grepl("below", al, ignore.case = TRUE)) { # cumulative should not have Below achievementLevel
          return(NULL)
        } else {
          lal <- paste0(ifelse(tolower(al) == tolower(names(als[length(als)])),"At ", "At or Above "),al)
          grep(lal,meanA$cumulative$Level, ignore.case = TRUE)
        }
      })
      if (length(lA) == 0) {
        stop("Achievement Level cannot be found in the results of the call of the function ",sQuote("achievementLevel"))
      }
      if (!skipB) {
        lB <- sapply(achievementLevel, function(al) {
          if (grepl("below", al, ignore.case = TRUE)) {
            lal <- al
          } else {
            lal <- paste0(ifelse(tolower(al) == tolower(names(als[length(als)])),"At ", "At or Above "),al)
          }
          grep(lal,meanB$cumulative$Level, ignore.case = TRUE)
        })
      }
      resA <- meanA$cumulative$Percent[lA]
      if (!skipB) {
        resB <- meanB$cumulative$Percent[lB]
        SEs <- list(estimateAse=meanA$cumulative$StandardError[lA],
                    estimateBse=meanB$cumulative$StandardError[lB])
      } else {
        SEs <- list(estimateAse=meanA$cumulative$StandardError[lA])
      }
    }
    
    # get Cov
    
    if (achievementDiscrete){
      maJK <- subset(meanA$discVarEstInputs$JK, variable %in% meanA$discrete$Level[lA])
      maJK$Level <- maJK$variable
      maJK$variable <- "A"
      if (!skipB) {
        mbJK <- subset(meanB$discVarEstInputs$JK, variable %in% meanB$discrete$Level[lB])
        mbJK$Level <- mbJK$variable
        mbJK$variable <- "B"
      }
    } # end if(achievementDiscrete)
    else {
      maJK <- subset(meanA$cumVarEstInputs$JK, variable %in% meanA$cumulative$Level[lA])
      maJK$Level <- maJK$variable
      maJK$variable <- "A"
      if (!skipB) {
        mbJK <- subset(meanB$cumVarEstInputs$JK, variable %in% meanB$cumulative$Level[lB])
        mbJK$Level <- mbJK$variable
        mbJK$variable <- "B"
      }
    } # end else if(achievementDiscrete)
    
    # calculate the difference
    if (!skipB) {
      mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate","Level"), suffixes=c(".A", ".B"))
      mdJK$variable <- "A-B"
      mdJK$value <- mdJK$value.A - mdJK$value.B
      mdJK$value.A <- mdJK$value.B <- NULL
      mdJK$variable.A <- mdJK$variable.B <- NULL
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
    # make varEstInputs$PV
    if (achievementDiscrete){
      statisticsLevelOut <- meanA$discrete$Level[lA]
      maPV <- subset(meanA$discVarEstInputs$PV, variable %in% meanA$discrete$Level[lA])
      maPV$Level <- maPV$variable
      maPV$variable <- "A"
      if (!skipB) {
        mbPV <- subset(meanB$discVarEstInputs$PV, variable %in% meanB$discrete$Level[lB])
        mbPV$Level <- mbPV$variable
        mbPV$variable <- "B"
      }  
    } # end if(achievementDiscrete)
    else {
      statisticsLevelOut <- meanA$cumulative$Level[lA]
      maPV <- subset(meanA$cumVarEstInputs$PV, variable %in% meanA$cumulative$Level[lA])
      maPV$Level <- maPV$variable
      maPV$variable <- "A"
      if (!skipB) {
        mbPV <- subset(meanB$cumVarEstInputs$PV, variable %in% meanB$cumulative$Level[lB])
        mbPV$Level <- mbPV$variable
        mbPV$variable <- "B"
      }
    } # end else if(achievementDiscrete)
    
    # calculate the difference
    if (!skipB) {
      mdPV <- merge(maPV, mbPV, by=c("PV","Level"), suffixes=c(".A", ".B"))
      mdPV$variable <- "A-B"
      mdPV$value <- mdPV$value.A - mdPV$value.B
      mdPV$value.A <- mdPV$value.B <- NULL
      mdPV$variable.A <- mdPV$variable.B <- NULL
      varEstPV <- rbind(maPV, mbPV, mdPV)
    } else {
      varEstPV <- maPV
    }
    
    # make varEstInputs
    row.names(varEstJK) <- NULL
    row.names(varEstPV) <- NULL
    varEstInputs <- list(JK=varEstJK, PV=varEstPV)
    
  } # end if (type == "AL")
  groupA_0 <- FALSE
  groupB_0 <- FALSE
  if(type == "eq") {
    # gap in percent in some covariate
    calleq <- c(callv, list(formula=formula(paste0(" ~ ", variable)),
                            returnMeans=FALSE,
                            pctAggregationLevel = NULL,
                            returnSepct = TRUE,
                            varMethod=varMethod,
                            drop = FALSE,
                            returnVarEstInputs=TRUE))
    calleqA <- c(calleq, list(data=dataA))
    meanA <- do.call(edsurveyTable, calleqA)
    
    if (!targetLevel %in% meanA$data[[variable]]) {
      groupA_0 <- TRUE
    }
    resA <- ifelse(groupA_0,0,meanA$data[meanA$data[variable] == targetLevel,4])
    if (!skipB) {
      calleqB <- c(calleq, list(data=dataB))
      meanB <- do.call(edsurveyTable, calleqB)
      if (!targetLevel %in% meanB$data[[variable]]) {
        groupB_0 <- TRUE
      }
      resB <- ifelse(groupB_0,0,meanB$data[meanB$data[variable] == targetLevel,4])
      SEs <- list(estimateAse=ifelse(groupB_0,NA,meanA$data[meanA$data[variable] == targetLevel,5]),
                  estimateBse=ifelse(groupB_0,NA,meanB$data[meanB$data[variable] == targetLevel,5]))
    } else {
      SEs <- list(estimateAse=ifelse(groupB_0,NA,meanA$data[meanA$data[variable] == targetLevel,5]))
    }
    
    # make varEstInputs$JK
    if (!groupA_0) {
      maJK <- meanA$pctVarEstInputs$JK[meanA$pctVarEstInputs$JK$variable == paste0(variable,"=", targetLevel),]
      maJK$variable <- "A"
    } else  {
      maJK <- NULL
    }
    if (!skipB) {
      if (!groupB_0) {
        mbJK <- meanB$pctVarEstInputs$JK[meanB$pctVarEstInputs$JK$variable == paste0(variable,"=", targetLevel),]
        mbJK$variable <- "B"
      } else {
        mbJK <- NULL
      }
      # calculate the difference
      if (!groupA_0 & !groupB_0) {
        mdJK <- merge(maJK, mbJK, by=c("PV", "JKreplicate"), suffixes=c(".A", ".B"))
        mdJK$variable <- "A-B"
        mdJK$value <- mdJK$value.A - mdJK$value.B
        mdJK$value.A <- mdJK$value.B <- NULL
        mdJK$variable.A <- mdJK$variable.B <- NULL
      } else {
        mdJK <- NULL
      }
      varEstJK <- rbind(maJK, mbJK, mdJK)
    } else {
      varEstJK <- maJK
    }
    
    # make varEstInputs
    if (!is.null(varEstJK)) {
      row.names(varEstJK) <- NULL
      varEstInputs <- list(JK=varEstJK, PV=NULL)
    } else {
      varEstInputs <- NULL
    }
    
  } # end if (type == "eq")
  
  # Compute difference statistics (diffAB, covAB, pooledse, pooleddf)
  if (!skipB) {
    diff <- unname(resA - resB) # unname removes the name (Intercept) that otherwise ends up in the retruned value
    nSize <- c(nrow(dataA),nrow(dataB))
    total <- which.max(nSize) # total is 1 if groupA is the total and 2 if groupB is the total
    p <- nrow(dataAB)/nSize[total]
    cov <- p * (SEs[[total]])^2
    wgtl <- getAttributes(data, "weights")[[wgt]]
    JRdfA <- JRdfB <- JRdf <- length(wgtl$jksuffixes)
    if(!is.null(varEstInputs)) {
      if (type %in% c("pct","AL")) {
        cov <- c()
        JRdfA <- c()
        JRdfB <- c()
        JRdf <- c()
        # we need to calculate cov by each Level
        for (si in statisticsLevelOut) {
          varEstInputsTemp <- list(JK=subset(varEstInputs$JK, Level %in% si),
                                   PV=subset(varEstInputs$PV, Level %in% si))
          if (nrow(varEstInputsTemp$JK) == 0) {
            varEstInputsTemp$JK <- NULL
          }
          if (nrow(varEstInputsTemp$PV) == 0) {
            varEstInputsTemp$PV <- NULL
          }
          if (!is.null(varEstInputsTemp$JK)) {
            cov <- c(cov, varEstToCov(varEstInputsTemp, 
                                      varA="A", varB="B", jkSumMultiplier=getAttributes(data, "jkSumMultiplier")))
            
            JRdfA <- c(JRdfA,DoFCorrection(varEstInputsTemp, varA="A", method=c("JR")))
            JRdfB <- c(JRdfB,DoFCorrection(varEstInputsTemp, varA="B", method=c("JR")))
            JRdf <- c(JRdf,DoFCorrection(varEstInputsTemp, varA="A", varB="B", method=c("JR")))   
          } # end if (!is.null(varEstInputsTemp$JK))
          else {
          	cov <- c(cov,NA)
          	JRdfA <- c(JRdfA,NA)
          	JRdfB <- c(JRdfB,NA)
          	JRdf <- c(JRdf,NA)
          }
        }
      } #end if (type %in% c("pct","AL")) 
      else {
        cov <- ifelse(groupA_0 | groupB_0,cov,varEstToCov(varEstInputs, varA="A", varB="B", jkSumMultiplier=getAttributes(data, "jkSumMultiplier")))
        JRdfA <- ifelse(groupA_0,NA,DoFCorrection(varEstInputs, varA="A", method=c("JR")))
        JRdfB <- ifelse(groupB_0, NA, DoFCorrection(varEstInputs, varA="B", method=c("JR")))
        JRdf <- ifelse(groupA_0 | groupB_0, NA, DoFCorrection(varEstInputs, varA="A", varB="B", method=c("JR"))) 
      }
      
    } # end if (!is.null(varEstInputs))
    diffSE <- mapply(function(a,b,c) {
      unname(sqrt(a^2 + b^2 - 2 * c))
    }, SEs[[1]],SEs[[2]],cov)
    diffSE <- unlist(diffSE)
    # get percent in each group
    
    # get variables used in conditions for groupA and groupB
    vn <- c(parseVars(substitute(groupA), data), parseVars(substitute(groupB), data))
    # add weights and variable itself
    vn <- c(vn, wgt, variable)
    # add Taylor variables when apropos
    taylorVars <- if(varMethod == "t") {
      vn <- c(wgt, c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar")))
    }
    
    # setup non-data call variables    
    callv <- list(varnames=vn,
                  drop=FALSE,
                  omittedLevels=omittedLevels,
                  recode=NULL) # recode is already done in line 335
    if(!missing(defaultConditions)) {
      callv <- c(callv, list(defaultConditions=defaultConditions))
    }
    
    # add weight call variables and do calls
    wgtcall0 <- c(callv, list(data=data))
    d0 <- do.call(getData, wgtcall0)
    wgtcallA <- c(callv, list(data=dataA))
    dA <- do.call(getData, wgtcallA)
    wgtcallB <- c(callv, list(data=dataB))
    dB <- do.call(getData, wgtcallB)
    pctA <- sum(dA[,wgt]) / sum(d0[,wgt])
    pctB <- sum(dB[,wgt]) / sum(d0[,wgt])
    # calculate covAB using JK method
    wgtl <- getAttributes(data, "weights")[[wgt]]
    pctJK <- t(sapply(wgtl$jksuffixes, function(suffix) {
      w0 <- sum(d0[,paste0(wgtl$jkbase, suffix)])
      wA <- sum(dA[,paste0(wgtl$jkbase, suffix)])
      wB <- sum(dB[,paste0(wgtl$jkbase, suffix)])
      c(pctA=wA/w0, pctB=wB/w0)
    }))
    seA <- 100 * sqrt(getAttributes(data, "jkSumMultiplier") * sum( (pctJK[,1] - pctA)^2 ))
    seB <- 100 * sqrt(getAttributes(data, "jkSumMultiplier") * sum( (pctJK[,2] - pctB)^2 ))
    #covAB <- 100 * 100 * sum( (pctJK[,1] - pctA) * (pctJK[,2] - pctB) )
    varJK <- data.frame(PV=rep(0, 3*nrow(pctJK)),
                        JKreplicate=rep(1:nrow(pctJK),3),
                        variable=rep(c("A", "B", "A-B"), each=nrow(pctJK)),
                        value=c(pctJK[,1] - pctA,
                                pctJK[,2] - pctB,
                                (pctJK[,1] - pctA) - (pctJK[,2] - pctB)
                        )
    )
    # there is no PV here because weights are not imputed
    pctVarEstInputs <- list(JK=varJK, PV=NULL)
    covAB <- varEstToCov(pctVarEstInputs, varA="A", varB="B", jkSumMultiplier=getAttributes(data, "jkSumMultiplier"))
    pctDoFA <- DoFCorrection(pctVarEstInputs, varA="A", method="JR")
    pctDoFB <- DoFCorrection(pctVarEstInputs, varA="B", method="JR")
    pctJRdf <- DoFCorrection(pctVarEstInputs, varA="A-B", method="JR")
    # end covAB calculation
    pctA <- 100 * pctA
    pctB <- 100 * pctB
    seAB <- unname(sqrt(seA^2 + seB^2 - 2 * 100 * 100 * covAB))
    
    # start return vector, first name achievement level
    vec <- switch(type,
                  pct=data.frame(percentiles=as.numeric(percentiles), stringsAsFactors=FALSE),
                  AL=data.frame(achievementLevel=gsub("Below","below",as.character(statisticsLevelOut)), stringsAsFactors=FALSE),
                  data.frame())
    if(returnSimpleDoF) {
      # add dofA and dofB if requested
      if(nrow(vec) > 0) {
        vec <- cbind(vec,
                     data.frame(estimateA=resA, estimateAse=SEs[[1]], dofA=JRdfA,
                                estimateB=resB, estimateBse=SEs[[2]], dofB=JRdfB))
      } else { # this is equivalent to  if (type %in%  c("mu","eq"))
        vec <- data.frame(estimateA=resA, estimateAse=SEs[[1]], dofA=JRdfA,
                          estimateB=resB, estimateBse=SEs[[2]], dofB=JRdfB)
      }
    } # end if(returnSimpleDoF) 
    else {
      # otherwise, no dofA or dofB
      if(nrow(vec) > 0) {
        vec <- cbind(vec,
                     data.frame(estimateA=resA, estimateAse=SEs[[1]],
                                estimateB=resB, estimateBse=SEs[[2]]))
      } else { # this is equivalent to  if (type %in%  c("mu","eq"))
        vec <- data.frame(estimateA=resA, estimateAse=SEs[[1]],
                          estimateB=resB, estimateBse=SEs[[2]])
      }
    }
    diffp <- 2*(1-pt2(abs(diff/diffSE), df=JRdf))
    vec <- cbind(vec,
                 data.frame(diffAB=diff, covAB=unname(cov),
                            diffABse= diffSE,
                            diffABpValue=diffp,
                            dofAB=JRdf))
    row.names(vec) <- NULL
    if(returnSimpleDoF) {
      percentage <- data.frame(pctA=pctA,
                               pctAse=seA,
                               dofA=pctDoFA,
                               pctB=pctB,
                               pctBse=seB,
                               dofB=pctDoFB)
    } else {
      percentage <- data.frame(pctA=pctA,
                               pctAse=seA,
                               pctB=pctB,
                               pctBse=seB)
    }
    pdiffp <- 2*(1-pt2(abs(pctA - pctB)/seAB, df=pctJRdf))
    percentage <- cbind(percentage,
                        data.frame(diffAB=pctA - pctB,
                                   covAB=100*100*covAB,
                                   diffABse=seAB,
                                   diffABpValue=pdiffp,
                                   dofAB=pctJRdf))
    
    lst <- list(results=vec,
                labels=list(A=substitute(groupA),B=substitute(groupB),
                            n0A=nrow2.edsurvey.data.frame(dataA),
                            n0B=nrow2.edsurvey.data.frame(dataB),
                            nUsedA=nrow(dA),
                            nUsedB=nrow(dB)),
                percentage=percentage)
  } else {
    wgtl <- getAttributes(data,"weights")[[wgt]]
    JRdfA <- ifelse(groupA_0,NA,length(wgtl$jksuffixes))
    if (!is.null(varEstInputs)) {
      if (type %in% c("pct","AL")) {
        JRdfA <- c()
        for (si in statisticsLevelOut) {
          varEstInputsTemp <- list(JK=subset(varEstInputs$JK, Level %in% si),
                                   PV=subset(varEstInputs$PV, Level %in% si))
          if (nrow(varEstInputsTemp$JK) == 0) {
            varEstInputsTemp$JK <- NULL
          }
          if (nrow(varEstInputsTemp$PV) == 0) {
            varEstInputsTemp$PV <- NULL
          }
          if (!is.null(varEstInputsTemp$JK)) {
            JRdfA <- c(JRdfA,DoFCorrection(varEstInputsTemp, varA="A", method=c("JR")))
          } else {
          	JRdfA <- c(JRdfA, NA)
          }
        }
      } #end if (type %in% c("pct","AL")) 
      else {
        JRdfA <- ifelse(groupA_0,NA,DoFCorrection(varEstInputs, varA="A", method=c("JR")))
      }
    }
    # get variables used in conditions for groupA and groupB
    vn <- c(parseVars(substitute(groupA), data), parseVars(substitute(groupB), data))
    # add weights and variable itself
    vn <- c(vn, wgt, variable)
    # add Taylor variables when apropos
    taylorVars <- if(varMethod == "t") {
      vn <- c(wgt, c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar")))
    }
    
    # setup non-data call variables    
    callv <- list(varnames=vn,
                  drop=FALSE,
                  #schoolMergeVarSchool=schoolMergeVarSchool,
                  #schoolMergeVarStudent=schoolMergeVarStudent,
                  omittedLevels=omittedLevels,
                  recode=NULL) # recode is already done in line 335
    if(!missing(defaultConditions)) {
      callv <- c(callv, list(defaultConditions=defaultConditions))
    }
    
    # add weight call variables and do calls
    wgtcall0 <- c(callv, list(data=data))
    d0 <- do.call(getData, wgtcall0)
    wgtcallA <- c(callv, list(data=dataA))
    dA <- do.call(getData, wgtcallA)
    pctA <- sum(dA[,wgt]) / sum(d0[,wgt])
    pctJK <- t(sapply(wgtl$jksuffixes, function(suffix) {
      w0 <- sum(d0[,paste0(wgtl$jkbase, suffix)])
      wA <- sum(dA[,paste0(wgtl$jkbase, suffix)])
      c(pctA=wA/w0, pctB = 0)
    }))
    seA <- 100 * sqrt(getAttributes(data, "jkSumMultiplier") * sum( (pctJK[,1] - pctA)^2 ))
    varJK <- data.frame(PV=rep(0, nrow(pctJK)),
                        JKreplicate=1:nrow(pctJK),
                        variable="A",
                        value= pctJK[,1] - pctA
    )
    # there is no PV here because weights are not imputed
    pctVarEstInputs <- list(JK=varJK, PV=NULL)
    pctDoFA <- DoFCorrection(pctVarEstInputs, varA="A", method="JR")
    pctA <- 100 * pctA
    # start return vector, first name achievement level
    vec <- switch(type,
                  pct=data.frame(percentiles=as.numeric(percentiles), stringsAsFactors=FALSE),
                  AL=data.frame(achievementLevel=gsub("Below","below",as.character(statisticsLevelOut)), stringsAsFactors=FALSE),
                  data.frame())
    if(returnSimpleDoF) {
      # add dofA and dofB if requested
      if(nrow(vec) > 0) {
        vec <- cbind(vec,
                     data.frame(estimateA=resA, estimateAse=SEs[[1]], dofA=JRdfA))
      } else {
        vec <- data.frame(estimateA=resA, estimateAse=SEs[[1]], dofA=JRdfA)
      }
    } else {
      # otherwise, no dofA or dofB
      if(nrow(vec) > 0) {
        vec <- cbind(vec,
                     data.frame(estimateA=resA, estimateAse=SEs[[1]]))
        
      } else {
        vec <- data.frame(estimateA=resA, estimateAse=SEs[[1]])
      }
    }
    row.names(vec) <- NULL
    if(returnSimpleDoF) {
      percentage <- data.frame(pctA=pctA,
                               pctAse=seA,
                               dofA=pctDoFA)
    } else {
      percentage <- data.frame(pctA=pctA,
                               pctAse=seA)
      
    }
    lst <- list(results=vec,
                labels=list(A=substitute(groupA),
                            n0A=nrow2.edsurvey.data.frame(dataA),
                            n0B=NA,
                            nUsedA=nrow(dA),
                            nUsedB=NA),
                percentage=percentage
                )
  }
  
  if(returnVarEstInputs) {
    if ("Level" %in% colnames(varEstInputs$JK)) { # equivalent to type = 'pct' or 'AL'
      if (type == 'pct') {
        varEstInputs$JK$Level <- as.numeric(gsub("^P","",varEstInputs$JK$Level))
        varEstInputs$PV$Level <- as.numeric(gsub("^P","",varEstInputs$PV$Level))
      }
      if (length(percentiles) <= 1 & length(achievementLevel) <= 1) {
        varEstInputs$JK$Level <- NULL
        varEstInputs$PV$Level <- NULL
      }
    }
    #assign values close to 0 to 0 
    varEstInputs$JK$value[which(abs(varEstInputs$JK$value) < (sqrt(.Machine$double.eps)*sqrt(nrow(varEstInputs$JK))))] <- 0 
    pctVarEstInputs$JK$value[which(abs(pctVarEstInputs$JK$value) < (sqrt(.Machine$double.eps)*sqrt(nrow(pctVarEstInputs$JK))))] <- 0 
    
    lst <- c(lst, list(varEstInputs=varEstInputs,
                       pctVarEstInputs=pctVarEstInputs))
  }
  class(lst) <- "gap"
  return(lst)
}

#' @rdname printGap
#' @title Gap Analysis Printing
#'
#' @description Prints labels and a results vector of a gap analysis.
#' @param x an R object representing a \code{gap} of class \code{gap} or \code{gapList}.
#' @param printPercentage a logical value set to \code{TRUE} to request printing 
#'                        of the percentage in the groups. Defaults to \code{TRUE}.
#' @param ... these arguments are not passed anywhere and are included only for compatibility.
#' @method print gap
#' @author Paul Bailey
#' @aliases print.gapList
#' @export
print.gap <- function(x, ..., printPercentage=TRUE) {
  cat("Call: ")
  print(x$call)
  cat("\nlabels:\n")
  lab <- data.frame(group=c("A", "B"),
                    definition=c(deparse(x$labels$A), deparse(x$labels$B)),
                    nFullData=c(x$labels$n0A, x$labels$n0B),
                    nUsed=c(x$labels$nUsedA, x$labels$nUsedB),
                    stringsAsFactors=FALSE)
  print(lab, row.names=FALSE)
  if(printPercentage) {
    cat("\npercentage:\n")
    print(x$percentage, row.names=FALSE)
  }
  cat("\nresults:\n")
  print(x$results, row.names=FALSE, ...)
}

#' @rdname printGap
#' @method print gapList
#' @export
print.gapList <- function(x, ..., printPercentage=TRUE) {
  cat("gapList\n")
  cat("Call: ")
  print(x$call)
  cat("\nlabels:\n")
  lab <- data.frame(group=c("A", "B"),
                    definition=c(deparse(x$labels$A), deparse(x$labels$B)),
                    stringsAsFactors=FALSE)
  print(lab, row.names=FALSE)
  if(printPercentage) {
    cat("\npercentage:\n")
    pct <- data.frame(x$percentage, stringsAsFactors=FALSE)
    print(pct, row.names=FALSE)
  }
  cat("\nresults:\n")
  print(x$results, row.names=FALSE)
}

# helper that identifies variables in a call
# ccall is the call to parse
# x is an edsurvey.data.frame or light.edsurvey.data.frame
# @author Paul Bailey
parseVars <- function(ccall,x) {
  vars <- c()
  # for each element
  if (typeof(ccall) == "symbol") {
    return(vars)
  }
  for(i in 1:length(ccall)) {
    # if it is a name
    if(class(ccall[[i]]) %in% c("name")) {
      ccall_c <- as.character(ccall[[i]])
      # if it is not in the data and is in the parent.frame, it might be a variable.
      if(ccall_c %in% names(x$data) ) {
        if (ccall[[i]] == "%in%" || is.function(ccall[[i]])) {
          # do nothing
        } else {
          vars <- c(vars, ccall_c)
        } 
      } # End of if statment: if (ccall_c %in% names(x$data))
    } # end if(class(ccall[[i]]) %in% c("name")) {
    if(class(ccall[[i]]) %in% "call") {
      # if this is a call, recursively parse that
      vars <- c(vars, parseVars(ccall[[i]], x))
    } #end of if statment: if class(ccall[[i]]) %in% "call"
  } # end of for loop: i in 1:length(ccall)
  vars
} # End of fucntion: parseVars

# helpter function that calculates "pooled" dof, used for two distinct samples
# @author Trang Nguyen and Paul Bailey
dofCompute <- function(seA,seB,dofA,dofB) {
  return(mapply(function(seA,seB,dofA,dofB) {
    ifelse(seA + seB > 0, (seA^2+seB^2)^2/(seA^4/dofA + seB^4/dofB), 0)
  }, seA,seB,dofA,dofB))
}

# returns NA when df is 0, but passes Info to pt to avoid a warning
# @author Paul Bailey
pt2 <- function(q, df) {
  df2 <- ifelse(df == 0, Inf, df)
  return(ifelse(df == 0, NA, pt(q, df2)))
}
