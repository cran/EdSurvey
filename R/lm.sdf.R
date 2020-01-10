#' @title EdSurvey Linear Models
#' @aliases lm
#' @description Fits a linear model that uses weights and variance estimates appropriate for the data.
#'
#' @param formula    a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}} for the
#'                   linear model. See \ifelse{latex}{\code{lm}}{\code{\link[stats]{lm}}}.
#'                   If \emph{y} is left blank, the default subject scale or subscale variable
#'                   will be used. (You can find the default using
#'                   \code{\link{showPlausibleValues}}.)
#'                   If \emph{y} is a variable for a subject scale or subscale (one of the
#'                   names shown by \code{\link{showPlausibleValues}}),
#'                   then that subject scale or subscale is used.
#' @param data       an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'                   or an \code{edsurvey.data.frame.list}
#' @param weightVar  a character indicating the weight variable to use (see Details).
#'                   The \code{weightVar} must be one of the weights for the
#'                   \code{edsurvey.data.frame}. If \code{NULL}, it  uses the default
#'                   for the \code{edsurvey.data.frame}.
#' @param varMethod  a character set to \dQuote{jackknife} or \dQuote{Taylor} that indicates the variance
#'                   estimation method to be used. See Details.
#' @param jrrIMax    a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the 
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \code{Vjrr} 
#'                   term (see 
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
#'                   can be estimated with any number of plausible values, and values larger than the number of 
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used. 
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param relevels   a list. Used to change the contrasts from the
#'                   default treatment contrasts to the treatment contrasts with a chosen omitted
#'                   group (the reference group). The name of each element should be the variable name, and the value 
#'                   should be the group to be omitted (the reference group).
#' @param omittedLevels a logical value. When set to the default value of \code{TRUE}, drops
#'                      those levels of all factor variables that are specified
#'                      in an \code{edsurvey.data.frame}. Use \code{print} on an
#'                      \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses
#'                          the default conditions stored in an \code{edsurvey.data.frame}
#'                          to subset the data. Use \code{print} on an
#'                          \code{edsurvey.data.frame} to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                  \code{recode=}\code{list(}\code{var1} \code{=} \code{list(}\code{from=} \code{c("a",} \code{"b",} \code{"c"),} \code{to=} \code{"d"))}. See Examples.
#' @param returnVarEstInputs a logical value set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates, which allow for the computation
#'                           of covariances between estimates.
#' @param returnNumberOfPSU a logical value set to \code{TRUE} to return the number of 
#'                          primary sampling units (PSUs)
#' @param standardizeWithSamplingVar a logical value indicating if the standardized coefficients
#'                                   should have the variance of the regressors and outcome measured
#'                                   with sampling variance. Defaults to \code{FALSE}.
#' @details 
#'  
#' This function implements an estimator that correctly handles left-hand
#' side variables that are either numeric or plausible values and allows for survey 
#' sampling weights and estimates variances using the jackknife replication method.
#' The vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}
#'  describes estimation of the reported statistics. 
#' 
#' Regardless of the variance estimation, the coefficients are estimated
#' using the sample weights according to the sections
#' \dQuote{Estimation of Weighted Means When Plausible Values Are Not Present}
#' or
#' \dQuote{Estimation of Weighted Means When Plausible Values Are Present,}
#' depending on if there are assessment variables or variables with plausible values
#' in them.
#' 
#' How the standard errors of the coefficients are estimated depends on the
#' value of \code{varMethod} and the presence of plausible values (assessment variables),
#' But once it is obtained, the \emph{t} statistic
#' is given by \deqn{t=\frac{\hat{\beta}}{\sqrt{\mathrm{var}(\hat{\beta})}}} where
#' \eqn{ \hat{\beta} } is the estimated coefficient and \eqn{\mathrm{var}(\hat{\beta})} is
#' the variance of that estimate.
#' 
#' The \bold{coefficient of determination (\emph{R}-squared value)} is similarly estimated by finding
#' the average \emph{R}-squared using the average across the plausible values.
#'
#' \subsection{Standardized regression coefficients}{
#'   Standardized regression coefficients can be returned in a call to \code{summary},
#'   by setting the argument \code{src} to \code{TRUE}. See Examples.
#' 
#'   By default, the standardized coefficients are calculated using standard
#'   deviations of the variables themselves, including averaging the standard
#'   deviation across any plausible values. When \code{standardizeWithSamplingVar}
#'   is set to \code{TRUE}, the variance of the standardized coefficient is
#'   calculated similar to a regression coefficient and therefore includes the
#'   sampling variance in the variance estimate of the outcome variable.
#' }
#'
#' \subsection{Variance estimation of coefficients}{
#'   All variance estimation methods are shown in the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}.
#'   When \code{varMethod} is set to the \code{jackknife} and the predicted
#'   value does not have plausible values, the variance of the coefficients
#'   is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When
#'         Plausible Values Are Not Present, Using the Jackknife Method.}
#'
#'   When plausible values are present and \code{varMethod} is \code{jackknife}, the
#'   variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When
#'         Plausible Values Are Present, Using the Jackknife Method.}
#'
#'   When plausible values are not present and \code{varMethod} is \code{Taylor}, the
#'   variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When Plausible
#'         Values Are Not Present, Using the Taylor Series Method.}
#'
#'   When plausible values are present and \code{varMethod} is \code{Taylor}, the
#'   variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When Plausible
#'         Values Are Present, Using the Taylor Series Method.}
#' }
#'
#' @section Testing:
#' Of the common hypothesis tests for joint parameter testing, only the Wald
#' test is widely used with plausible values and sample weights. As such, it
#' replaces, if imperfectly, the Akaike Information Criteria (AIC), the
#' likelihood ratio test, chi-squared, and analysis of variance (ANOVA, including \emph{F}-tests). See \code{\link{waldTest}} or
#' the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-WaldTest.pdf}{\emph{Methods and Overview of Using EdSurvey for Running Wald Tests}}.
#'
#' @references
#' Binder, D. A. (1983). On the variances of asymptotically normal estimators from complex surveys. \emph{International Statistical Review}, \emph{51}(3), 279--292. 
#'
#' Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York, NY: Wiley.
#'
#' van Buuren, S. (2012). \emph{Flexible imputation of missing data}. New York, NY: CRC Press.
#'
#' Weisberg, S. (1985). \emph{Applied linear regression} (2nd ed.). New York, NY: Wiley.
#'
#' @return
#' An \code{edsurvey.lm} with the following elements:
#'    \item{call}{the function call}
#'    \item{formula}{the formula used to fit the model}
#'    \item{coef}{the estimates of the coefficients}
#'    \item{se}{the standard error estimates of the coefficients}
#'    \item{Vimp}{the estimated variance from uncertainty in the scores (plausible value variables)}
#'    \item{Vjrr}{the estimated variance from sampling}
#'    \item{M}{the number of plausible values}
#'    \item{varm}{the variance estimates under the various plausible values}
#'    \item{coefm}{the values of the coefficients under the various plausible values}
#'    \item{coefmat}{the coefficient matrix (typically produced by the summary of a model)}
#'    \item{r.squared}{the coefficient of determination}
#'    \item{weight}{the name of the weight variable}
#'    \item{npv}{the number of plausible values}
#'    \item{jrrIMax}{the \code{jrrIMax} value used in computation}
#'    \item{njk}{the number of the jackknife replicates used; set to \code{NA}
#'               when Taylor series variance estimates are used}
#'    \item{varMethod}{one of \code{Taylor series} or the \code{jackknife}}
#'    \item{residuals}{residuals from the average regression coefficients}
#'    \item{PV.residuals}{residuals from the by plausible value coefficients}
#'    \item{PV.fitted.values}{fitted values from the by plausible value coefficients}
#'    \item{B}{imputation variance covariance matrix, before multiplication by (M+1)/M}
#'    \item{U}{sampling variance covariance matrix}
#'    \item{rbar}{average relative increase in variance; see van Buuren (2012, eq. 2.29)}
#'    \item{nPSU}{number of PSUs used in calculation}
#'    \item{n0}{number of rows on an \code{edsurvey.data.frame} before any conditions were applied}
#'    \item{nUsed}{number of observations with valid data and weights larger than zero}
#'    \item{data}{data used for the computation}
#'    \item{Xstdev}{standard deviations of regressors, used for computing standardized
#'                  regression coefficients when \code{standardizeWithSamplingVar} is set to
#'                  \code{FALSE} (the default)}
#'    \item{varSummary}{the result of running \code{summary2} (unweighted) on each variable in the
#'                      regression}
#'    \item{varEstInputs}{when \code{returnVarEstInputs} is \code{TRUE},
#'                        this element is returned. These are
#'                        used for calculating covariances with
#'                        \code{\link{varEstToCov}}.}
#'    \item{standardizeWithSamplingVar}{when \code{standardizeWithSamplingVar}
#'                                      is set to \code{TRUE}, this element is
#'                                      returned. Calculates the standard deviation
#'                                      of the standardized
#'                                      regression coefficients like any other
#'                                      variable.}
#'
#' @seealso \ifelse{latex}{\code{lm}}{\code{\link[stats]{lm}}}
#' @author Paul Bailey
#'
#' @example \man\examples\lm.sdf.R
#' @importFrom Matrix sparse.model.matrix rankMatrix
#' @importFrom stats lm aggregate pt relevel model.matrix lm.wfit as.formula complete.cases
#' @importFrom Formula Formula
#' @importFrom MASS ginv
#' @method lm sdf
#' @export
#' @export lm.sdf
#' @usage lm.sdf(formula, data, weightVar = NULL, relevels = list(),
#'               varMethod = c("jackknife", "Taylor"), jrrIMax = 1,
#'               omittedLevels = TRUE, defaultConditions = TRUE, recode = NULL,
#'               returnVarEstInputs = FALSE, returnNumberOfPSU = FALSE,
#'               standardizeWithSamplingVar = FALSE)
lm.sdf <- function(formula,
                   data,
                   weightVar=NULL,
                   relevels=list(),
                   varMethod=c("jackknife", "Taylor"),
                   jrrIMax=1,
                   omittedLevels=TRUE,
                   defaultConditions=TRUE,
                   recode=NULL,
                   returnVarEstInputs=FALSE,
                   returnNumberOfPSU=FALSE,
                   standardizeWithSamplingVar=FALSE) {
  call <- match.call()
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  # if data is an edsurvey.data.frame.list, simply return a list with results
  # for each edsurvey.data.frame
  if (inherits(data, c("edsurvey.data.frame.list"))) {
    res <- itterateESDFL(match.call(),data)
    class(res) <- "edsurveyLmList"
    return(res)
  } else {
    return(calc.lm.sdf(formula=formula,
                       data=data,
                       weightVar=weightVar,
                       relevels=relevels,
                       varMethod=varMethod,
                       jrrIMax=jrrIMax,
                       omittedLevels=omittedLevels,
                       defaultConditions=defaultConditions,
                       missingDefaultConditions=missing(defaultConditions),
                       recode=recode,
                       returnVarEstInputs=returnVarEstInputs,
                       returnLm0=FALSE,
                       call=call,
                       returnNumberOfPSU=returnNumberOfPSU,
                       standardizeWithSamplingVar=standardizeWithSamplingVar))
  }
}

calc.lm.sdf <- function(formula,
                        data,
                        weightVar=NULL,
                        relevels=list(),
                        varMethod=c("jackknife", "Taylor", "j", "t"),
                        jrrIMax=1,
                        omittedLevels=TRUE,
                        defaultConditions=TRUE,
                        missingDefaultConditions=TRUE,
                        recode=NULL,
                        returnVarEstInputs=FALSE,
                        returnLm0=FALSE,
                        call=NULL,
                        returnNumberOfPSU=FALSE,
                        standardizeWithSamplingVar=FALSE) {
  if(is.null(call)) {
    call <- match.call()
  }
  #######################
  ######## Outline ######
   #######################
  # 1) check, format inputs
  # 2) get the data
  # 3) deal with relevels.
  # 4) deal with yvar having plausible values
  # 5) run the main regression
  # 6) run the regressions, form the inputs for variance estimation
  # 7) form output, including final variance estimation

  # 1) check, format inputs
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  sdf <- data # short for survey data.frame
  
  # get varMethod, turn into first character as lower case
  varMethod <- substr(tolower(varMethod[[1]]), 0, 1)
  if(!varMethod %in% c("j", "t")) {
    stop(paste0("The argument ", sQuote("varMethod"), " must be one of ", dQuote("Taylor"), " or ", dQuote("jackknife"), "."))
  }
  if(standardizeWithSamplingVar & varMethod == "t") { 
    warning(paste0(sQuote("standardizeWithSamplingVar"), " reset to ", dQuote("FALSE"), " because ", sQuote("varMethod"), " is ", dQuote("Taylor"), "."))
    standardizeWithSamplingVar <- FALSE
  }

  # if the weight var is not set, use the default
  if(is.null(weightVar)) {
    wgt <- attributes(getAttributes(sdf, "weights"))$default
  } else {
    wgt <- weightVar
  } # End of if/else: is.null(weightVar)
  if(min(nchar(wgt)) == 0) {
    # no weight
    stop(paste0("There is no default weight variable for ",getAttributes(sdf,"survey")," data, so the argument ",sQuote("weightVar"), " must be specified."))
  }
  # check if there is an outcome variable and set it to the default if it is missing
  zeroLengthLHS <- attr(terms(formula), "response") == 0
  if(zeroLengthLHS) {
    yvar <- attributes(getAttributes(sdf, "pvvars"))$default
    formula <- update(formula, new=substitute( yvar ~ ., list(yvar=as.name(yvar))))
  } else{
    yvar <- all.vars(formula[[2]])
  } # End of If/Else: if (zeroLengthLHS)

  # grab the variables needed for the Taylor series method, if that is the variance estimation method being used
  taylorVars <- c()
  psuVar <- getPSUVar(data, wgt)
  stratumVar <- getStratumVar(data, wgt)
  if (is.null(psuVar)) {
    if(returnNumberOfPSU | varMethod=="t") {
      stop(paste0("Cannot find primary sampling unit variable for weight ", sQuote(wgt), ". Try setting the ", dQuote("varMethod"), " argument to ", dQuote("jackknife"), " and ", dQuote("returnNumberOfPSU"), " to ", dQuote("FALSE"), "."))
    } else {
      # set to a dummy variable, not on the data
      psuVar <- ""
    }
  }
  if (is.null(stratumVar)) {
    if(returnNumberOfPSU | varMethod=="t") {
      stop(paste0("Cannot find stratum variable for weight ", sQuote(wgt), ". Try setting the ", dQuote("varMethod"), " argument to ", dQuote("jackknife"), " and ", dQuote("returnNumberOfPSU"), " to ", dQuote("FALSE"), "." ))
    } else {
      # set to a dummy variable, not on the data
      stratumVar <- ""
    }
  }
  # in PIAAC there is sometimes an SRS, that is not appropriate for Taylor
  if(stratumVar == "JK1" & varMethod == "t") {
    varMethod <- "j"
    warning("Cannot use Taylor series estimation on a one-stage simple random sample.")
  }
  if(varMethod=="t") {
    taylorVars <- c(psuVar, stratumVar)
    jrrIMax <- NA
  } else {
    if(all(c(psuVar, stratumVar) %in% colnames(data))) {
      taylorVars <- c(psuVar, stratumVar)
    }
  }
  formulaVars <- all.vars(formula)
  getDataVarNames <- c(all.vars(formula), wgt, taylorVars)
  if (returnNumberOfPSU){
    # Get stratum and PSU variable
    stratumVar <- getStratumVar(data, wgt)
    psuVar <- getPSUVar(data, wgt)
    if (all(c(stratumVar, psuVar) %in% names(data)) | all(c(stratumVar, psuVar) %in% colnames(data))) { #names(data$data) changed to colnames(data)::Tom
      getDataVarNames <- unique(c(getDataVarNames,stratumVar,psuVar))
    } else {
      warning(paste0("Stratum and PSU variable are required for this call and are not on the incoming data. Resetting ", sQuote("returnNumberOfPSU"), " to ", dQuote("FALSE"), "."))
      returnNumberOfPSU <- FALSE
    }
  }
  # 2) get the data
  # This is most of the arguments
  getDataArgs <- list(data=sdf,
                      varnames=getDataVarNames,
                      returnJKreplicates=(varMethod=="j"),
                      drop= FALSE,
                      omittedLevels=omittedLevels,
                      recode = recode,
                      includeNaLabel=TRUE,
                      dropUnusedLevels=TRUE,
                      addAttributes=TRUE)

  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if(!missingDefaultConditions) {
    getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
  }

  # edf is the actual data
  edf <- do.call(getData, getDataArgs)
  # check for incomplete cases on the formula variables and weights (excluding Taylor vars)
  relVars <- colnames(edf)
  relVars <- relVars[!relVars %in% taylorVars]
  incomplete <- !complete.cases(edf[,relVars])
  if(any(incomplete)) {
    warning("Removing ", sum(incomplete), " rows with NAs from analysis.")
    edf <- edf[!incomplete,]
  }
  # if doing Taylor series, check Taylor vars too
  if(varMethod=="t") {
    incomplete <- !complete.cases(edf[,taylorVars])
    if(any(incomplete)) {
      warning("Removing ", sum(incomplete), " rows with NA PSU of stratum variables from analysis.")
      edf <- edf[!incomplete,]
    }
  }
  
  # remove non-positive (full sample) weights
  if(any(edf[,wgt] <= 0)) {
    posWeights <- edf[,wgt] > 0
    warning("Removing ", sum(!posWeights), " rows with nonpositive weight from analysis.")
    edf <- edf[posWeights,]
  }
  
  # check that there is some data to work with
  if(nrow(edf) <= 0) {
    stop(paste0(sQuote("data"), " must have more than 0 rows after a call to ",
                sQuote("getData"), "."))
  }
  
  # 3) deal with relevels.
  # An argument that allows the user to change the omitted level for a factor variable
  if(length(relevels) > 0) {
    for(i in 1:length(relevels)) {
      vari <- names(relevels)[i]
      if(! vari %in% names(edf)) {
        stop(paste0("In the ", sQuote("relevels"),
                    " argument, cannot find the variable named ",sQuote(vari),"."))
      } # End of if statment: ! vari %in% names(edf)
      if(length(relevels[[i]]) != 1) {
        stop(paste0("In the ", sQuote("relevels"),
                    " argument, each relevel must have exactly one level."))
      } # End of if statment: length(relevels[[i]]) != 1
      # check that the level exists
      lvls <- levels(edf[,vari])
      if(inherits(edf[,vari], "lfactor")) {
        # for a factor it can be either a level or a label
        lvls <- c(lvls, labels(edf[,vari]))
      }# End of if statment: inherits(edf[,vari], "lfactor")
      if(!relevels[[i]] %in% lvls){
        stop(paste0("In the ", sQuote("relevels"),
                    " argument, for the variable ", sQuote(vari), ", the level ",
                    sQuote(relevels[[i]]) , " not found. Found levels are ",
                    pasteItems(dQuote(lvls)), "."))
      } # End of if statment !relevels[[i]] %in% lvls
      edf[,vari] <- relevel(edf[,vari], ref=relevels[[i]])
    } # end for(i in 1:length(relevels))
  } # end if(length(relevels) > 0) 
  
  # droplevel on covariates to avoid problems with X is formed later.
  # also, check that there are not factors with only one level and give more informative error. 
  for(i in 1:ncol(edf)) {
    if(inherits(edf[,i], "factor") & colnames(edf)[i] %in% formulaVars) {
      edf[,i] <- droplevels(edf[,i])
      if(length(levels(edf[,i])) <2) {
        stop(paste0("The covariate ", dQuote(colnames(edf)[i]),
                    " has fewer than two levels after zero-weight cases are dropped. This covariate cannot be included in the regression."))
      }
    }
  }
  varSummary <- lapply(formulaVars, function(x) summary2(data=edf, variable=x, weightVar=NULL, omittedLevels=FALSE))
  
  # 4) deal with yvar having plausible values
  pvy <- hasPlausibleValue(yvar, sdf) # pvy is the plausible values of the y variable
  yvars <- yvar
  lyv <- length(yvars)
  if(any(pvy)) {
    yvars <- paste0("outcome",1:length(getPlausibleValue(yvars[max(pvy)], data)))
  } else {
    # no PVs, just make sure that this variable is numeric
    edf[,"yvar"] <- as.numeric(eval(formula[[2]],edf))
    formula <- update(formula, new=substitute( yvar ~ ., list(yvar=as.name(yvar))))
    yvars <- "yvar"
  } # End of if statment: any(pvy)

  yvar0 <- yvars[1]
  
  # this allows that variable to not be dynamic variable, it is explicitly defined to be yvar0
  if(any(pvy)) {
    for(i in 1:length(yvars)) {
      # PV, so we have not evaluated the I() yet (if any)
      # first, by PV, rename the ith PVs to be e.g. composite
      for(yvi in 1:length(pvy)) {
        if(pvy[yvi]) {
          edf[,yvar[yvi]] <- edf[,getPlausibleValue(yvar[yvi], data)[i]]
        }
      }
      # then set yvars[i] (e.g. outcome1) to be the evaluation at this PV point
      edf[,yvars[i]] <- as.numeric(eval(formula[[2]],edf))
    }
    # finally, correctly set yvar0
    edf$yvar0 <- edf[,yvar0]
  } # end if(any(pvy)), no else
  
  # 5) run the main regression

  # run a regression, starting with the first PV or maybe the only outcome variable
  yvar0 <- yvars[1]
  # this allows that variable to not be dynamic variable, it is explicitly defined to be yvar0
  edf$yvar0 <- edf[,yvar0]
  frm <- update(formula, yvar0 ~ .)
  edf$w <- edf[,wgt]
  lm0 <- lm(frm, data=edf, weights=w)
  if(returnLm0) {
    return(lm0)
  }

  # get standard devaitions for future standard mean difference (SMD)
  # calculation
  X <- model.matrix(frm, edf)
  # fill this in regardless of standardizeWithSamplingVar to make a "shell"
  std <- getStdev(edf[,yvars], X, edf[,wgt])

  # this grabs the list of weight variable names
  wgtl <- getAttributes(sdf, "weights")[[wgt]]
  varEstInputs <- list()
  # note we have not made the B matrix
  madeB <- FALSE
  # setup std as a matrix we can condense.
  # there is now a very long set of conditionals
  # 6) run the regressions, form the inputs for variance estimation
  if(any(pvy)) { # the y variable is a plausible value
    # this if condition takes care of the Taylor series method as well as the jackknife method
    # for equations, see the statistics vignette
    jrrIMax <- min(jrrIMax, length(yvars))
    if(varMethod=="t") {
      jrrIMax <- length(yvars)
    } 
    # varm is the variance matrix by coefficient and PV (for V_jrr)
    varm <- matrix(NA, nrow=jrrIMax, ncol=length(coef(lm0)))
    varM <- list()
    # coefficients by PV
    coefm <- matrix(NA, nrow=length(yvars), ncol=length(coef(lm0)))
    if(standardizeWithSamplingVar) {
      varmS <- varm
      coefmS <- coefm
    }
    # R-squared by PV
    r2s <- vector(length=length(yvars))
    if(varMethod == "j") {
      # y is a variable with plausible values and we are using the jackknife apporach
      for(pvi in 1:jrrIMax) { # for each PV (up to jrrIMax)
        coefa <- matrix(NA, nrow=length(wgtl$jksuffixes), ncol=length(coef(lm0)))
        if(standardizeWithSamplingVar) {
          coefS <- coefa
        }
        edf$yvar0 <- edf[,yvars[pvi]]
        edf$w <- edf[,wgt]
        lmi <- lm(frm, data=edf, weights=w)
        co0 <- coef(lmi)
        if(standardizeWithSamplingVar) {
          coS0 <- standardizeCoef(co0, std)
          coefmS[pvi,] <- coS0
        }
        r2s[pvi] <- summary(lmi)$r.squared
        coefm[pvi,] <- co0
        X_lmi <- model.matrix(frm, edf)
        Y_lmi <- edf[,as.character(frm[[2]])]
        for(jki in 1:length(wgtl$jksuffixes)) {
          w_lmi <- edf[,paste0(wgtl$jkbase, wgtl$jksuffixes[jki])]
          lmi <- lm.wfit(x = X_lmi, y = Y_lmi, w = w_lmi)
          coefa[jki,] <- coef(lmi)
          if(standardizeWithSamplingVar) {
            stdij <- getStdev(edf[,yvar0], X, w_lmi)
            coefi <- coefa[jki,]
            names(coefi) <- names(co0)
            coefS[jki,] <- standardizeCoef(coefi, stdij)
          }
        } # End of for loop: jki in 1:length(wgtl$jksuffixes)
        coefa <- t( (t(coefa) - co0))
        if(standardizeWithSamplingVar) {
          coefS <- t( (t(coefS) - coS0) )
          # correctly uses the coefa cutoff
          coefS[ 2*abs(co0 * .Machine$double.eps) > abs(coefa)] <- 0
          coefS <- coefS^2
          varmS[pvi,] <- getAttributes(data, "jkSumMultiplier") * apply(coefS, 2, sum)
        }
        # find computational zeros and set them to actual zero
        coefa[ 2*abs(co0 * .Machine$double.eps) > abs(coefa)] <- 0
        frm <- Formula(frm)
        # except for the null model, check for lack of variance across strata
        # null model is used in edsurveyTable and gives mean estimates
        if(terms(frm, lhs = 0, rhs = NULL) != ~1){
          if(sum(!is.na(coefa))==0 || sum(coefa,na.rm=TRUE) == 0) {
            stop("No variance across strata. This could be due to only one stratum being included in the sample.")
          }
        }

        # store results for building varEstInputs$JK (veiJK)
        dfl <- lapply(1:ncol(coefa), function(coli) {
          data.frame(PV=rep(pvi, nrow(coefa)),
                     JKreplicate=1:nrow(coefa),
                     variable=names(coef(lmi))[coli],
                     value=coefa[,coli])
        })
        # veiJK is the JK part of varEstInputs
        if(pvi == 1) {
          veiJK <- do.call(rbind, dfl)
        } else {
          veiJK <- rbind(veiJK, do.call(rbind, dfl))
        }
        # varm is now the diagonal of this. Notice that this only uses first 
        # jrrIMax PVs 
        varM[[pvi]] <- getAttributes(data, "jkSumMultiplier") * 
                       Reduce("+",
                              lapply(1:nrow(coefa), function(jki) {
                                cc <- coefa[jki,]
                                cc[is.na(cc)] <- 0
                                outer(cc, cc)
                              }))
        coefa <- coefa^2
        varm[pvi,] <- getAttributes(data, "jkSumMultiplier") * apply(coefa, 2, sum)
      } # End of for loop: (pvi in 1:jrrIMax)
        
      varEstInputs[["JK"]] <- veiJK
      while(pvi < length(yvars)) {
        pvi <- pvi + 1
        edf$yvar0 <- edf[,yvars[pvi]]
        edf$w <- edf[,wgt]
        co0 <- coef(lmi <- lm(frm, data=edf, weights=w))
        r2s[pvi] <- summary(lmi)$r.squared
        coefm[pvi,] <- co0
        if(standardizeWithSamplingVar) {
          #stdi <- getStdev(edf[,yvar0], X, edf$w) # global weight
          coS0 <- standardizeCoef(co0, std)
          coefmS[pvi,] <- coS0
        }
      } # End while loop: pvi < length(yvars)
    } else { # End of if statment: varMethod == "j"
      # Taylor series, PVs
      # y is a variable with plausible values and we are using the Taylor series approach
      # 
      # here the regression uses the normal equations for weighted regression
      # the QR decomposition is used to improve stability but
      # the D matrix involves an inverse that cannot be avoided
      X <- sparse.model.matrix(frm, edf)
      XW <- X * sqrt(edf[,wgt,drop=TRUE])
      qrXW <- qr(XW)
      W <- Diagonal(n=nrow(edf),edf[,wgt,drop=TRUE])
      D <- solve(t(X) %*% W %*% X)
      dofNum <- matrix(0, nrow=nrow(D), ncol=length(yvars))
      dofDenom <- matrix(0, nrow=nrow(D), ncol=length(yvars))
      warnDrop <- 0 # warn if a stratum is too thin to estimate variance on it
      lms <- lapply(1:length(yvars), function(mm) {
        # for each PV, run the regression, form the coefficients and variance components
        Y <- as(edf[,yvars[mm],drop=FALSE],"Matrix")
        YW <- Y * sqrt(edf[,wgt,drop=TRUE])
        # next line gets b (the coefficients) in the traditional 
        # but less stable way than the line after it, which is based on QR
        #b <- D %*% t(X) %*% W %*% Y
        b <- qr.coef(qrXW, YW)
        # get residual
        fitted <- X%*%b
        e <- as.vector((Y-fitted))
        # notation (uhij) from AM documentation
        # this is the partial of the likelihood at the unit level
        uhij <- e*as.matrix(X)
        # singleton PSUs removed in lapply below, not here
        for(bi in 1:length(b)) { # for each coefficient
          coln <- colnames(uhij)[bi]
          # get the stratum/PSU based sum
          edf$bb <- uhij[,bi] * edf[,wgt]
          taylorVars <- c(psuVar, stratumVar)
          res_sp <- aggregate(formula(paste0("bb ~ ", psuVar, " + ", stratumVar)), edf, sum)
          # and the average of the previous line results across strata (notice data=res_sp)
          res_s <- aggregate(formula(paste0("bb ~ ", stratumVar)), res_sp, mean)
          names(res_sp)[3] <- coln
          names(res_s)[2] <- paste0("uh_",coln)
          # grab the sum of weights, this does not vary by regression coef, so only do this once
          if(bi==1) {
            uhi <- res_sp
            edf$strtWgt <- edf[,wgt]
            res_s_wgt <- aggregate(formula(paste0("strtWgt ~ ", stratumVar)), edf, sum)
            uhi <- merge(uhi, res_s_wgt, by=c(stratumVar), all=TRUE)
          } else{
            uhi <- merge(uhi, res_sp, by=c(psuVar, stratumVar), all=TRUE)
          }
          uhi <- merge(uhi, res_s, by=c(stratumVar), all=TRUE)
          uhi[,paste0("dx",bi)] <- uhi[,coln] - uhi[,paste0("uh_",coln)]
        } #end of for(bi in 1:length(b)) 

        # this will be the variance-covariance matrix for this plausible value
        vv <- matrix(0,nrow=length(b), ncol=length(b))
        sa <- lapply(unique(uhi[,stratumVar]), function(ii) {
          vvj <- matrix(0,nrow=length(b), ncol=length(b))
          unkj <- unique(uhi[uhi[,stratumVar] == ii, psuVar, drop=TRUE])
          if(length(unkj)>1) { # cannot estimate variance off one PSU
            sb <- lapply(unkj, function(jj) {
              v <- as.numeric(t(uhi[uhi[,stratumVar]==ii & uhi[,psuVar]==jj, paste0("dx",1:length(b)), drop=FALSE]))
              vvj <<- vvj + v %*% t(v)
            })
            vvj <- vvj * ( (length(unkj)) / ( length(unkj) - 1) )
            vv <<- vv + vvj
            num <- diag(D %*% vvj %*% D)
            dofNum[,mm] <<- dofNum[,mm] + num
            dofDenom[,mm] <<- dofDenom[,mm] + num^2
          } else { # End of if statment: if(length(unkj)>1)
            warnDrop <<- warnDrop + 1
          }
        })
        M <- vv
        vc <- D %*% M %*% D
        # get R-squared
        meanY <- sum(W %*% Y) / sum(diag(W))
        dY <- Y-meanY
        syy <- t(dY) %*% W %*% dY # Weisberg, and adding in weights
        rss <- t(e) %*% W %*% e # Weisberg, page 81 eq 4.2
        r2s[mm] <<- as.numeric(1-(rss/syy)) # Weisberg, page 49 eq 2.31
        coefm[mm,] <<- as.numeric(b)
        varM[[mm]] <<- as.matrix(vc)
        varm[mm,] <<- as.numeric(diag(vc))
      })
      if(warnDrop > 0) {
        warning(paste0(warnDrop/length(yvars), " strata had only one populated PSU. Units in these strata assumed to be certainties. You can condense the strata/PSU structure to avoid this."))
      }

    } # End of if/else statment: varMethod == "j"
    # number of PVs
    M <- length(yvars)
    # imputaiton variance / variance due to uncertaintly about PVs

    coefm0 <- t(t(coefm) - apply(coefm, 2, mean))
    # calculate van Buuren B
    B <- (1/(M-1))* Reduce("+", # add up the matrix results of the sapply
                           sapply(1:nrow(coefm), function(q) {
                             # within each PV set, calculate the outer product
                             # (2.19 of Van Buuren)
                             outer(coefm0[q,],coefm0[q,])
                           }, simplify=FALSE)
                          )
    madeB <- TRUE
    # \bar{U} from 2.18 in var Buuren 
    Ubar <- (1/length(varM)) * Reduce("+", varM)

    Vimp <- (M+1)/M * apply(coefm, 2, var)

    r2 <- mean(r2s)
    coef <- apply(coefm, 2, mean)
    # variance due to sampling
    Vjrr <- apply(varm[1:jrrIMax,,drop=FALSE], 2, mean)
    V <- Vimp + Vjrr
    coefmPV <- t( t(coefm) - apply(coefm, 2, mean))
    dfl <- lapply(1:ncol(coefmPV), function(coli) {
      data.frame(PV=1:nrow(coefmPV),
                 variable=rep(names(coef(lm0))[coli], nrow(coefmPV)),
                 value=coefmPV[,coli])
    })
    coefmPV <- do.call(rbind, dfl)
    varEstInputs[["PV"]] <- coefmPV
    if(standardizeWithSamplingVar) {
      VSjrr <- apply(varmS[1:jrrIMax,,drop=FALSE], 2, mean)
      VSimp <- (M+1)/M * apply(coefmS, 2, var)
      VS <- VSimp + VSjrr
    }
  } else { # end if(pvy)
    # the y variable is not a plausible value
    # this section handles jackknife and Taylor series estimation
    if(varMethod == "j") {
      # jackknife variance estimation
      coefa <- matrix(NA, nrow=length(wgtl$jksuffixes), ncol=length(coef(lm0)))
      co0 <- coef(lm0)
      r2 <- summary(lm0)$r.squared
      
      X_lmi <- model.matrix(frm, edf)
      Y_lmi <- edf[,as.character(frm[[2]])]
      for(jki in 1:length(wgtl$jksuffixes)) {
        w_lmi <- edf[,paste0(wgtl$jkbase, wgtl$jksuffixes[jki])]
        lmi <- lm.wfit(x = X_lmi, y = Y_lmi, w = w_lmi)
        coefa[jki,] <- coef(lmi)
      }
      coefa <- t( (t(coefa) - co0)) # conservative JK estimator
      # gt var est inputs
      dfl <- lapply(1:ncol(coefa), function(coli) {
        data.frame(PV=rep(1, nrow(coefa)),
                   JKreplicate=1:nrow(coefa),
                   variable=names(coef(lmi))[coli],
                   value=coefa[,coli])
      })
      veiJK <- do.call(rbind, dfl)
      varEstInputs[["JK"]] <- veiJK
      # covariance matrix is the outer
      Ubar <- getAttributes(data, "jkSumMultiplier") * 
                Reduce("+",
                  lapply(1:nrow(coefa), function(jki) {
                    cc <- coefa[jki,]
                    cc[is.na(cc)] <- 0
                    outer(cc, cc)
                  }))
      # done getting var est inputs
      njk <- length(wgtl$jksuffixes)
      coefa <- coefa^2 # this is JK-2
      Vjrr <- getAttributes(data, "jkSumMultiplier") * apply(coefa, 2, sum, na.rm=TRUE)
      coef <- co0 # coefficients come from full sample weights run
      Vimp <- 0 # no imputation variance when there are no PVs
      M <- 1 # only on replicate when there are no PVs
      varm <- NULL # no variance by PV
      coefm <- NULL # no coefficients matrix by PV

    } else { # end if(varMethod == "j")
      # Taylor series, no PV
      Y <- as(edf[,yvars[1],drop=FALSE],"Matrix")
      X <- sparse.model.matrix(frm, edf)
      W <- Diagonal(n=nrow(edf),edf[,wgt,drop=TRUE])
      D <- vcov(lm0)
      b <- coef(lm0)
      # fitted values
      fitted <- X%*%b
      # residuals, devide by sigma^2 because D from vcov(lm0) has a sigma2 in it
      e <- (as.vector((Y-fitted)))/summary(lm0)$sigma^2
      # this is the partial of the likelihood at the unit level
      uhij <- e*as.matrix(X)
      # singletons dropped in lapply below, not here
      for(bi in 1:length(b)) { # for each coefficient
        coln <- colnames(uhij)[bi]
        # get the stratum/PSU based sum
        edf$bb <- uhij[,bi] * edf[,wgt]
        resi <- aggregate(formula(paste0("bb ~ ", psuVar, " + ", stratumVar)), edf, sum)
        # and the average of the same across strata
        resj <- aggregate(formula(paste0("bb ~ ", stratumVar)),resi,function(x) { mean(x)})
        names(resi)[3] <- coln
        names(resj)[2] <- paste0("uh_",coln)
        if(bi==1) {
          uhi <- resi
          edf$strtWgt <- edf[,wgt]
          resw <- aggregate(formula(paste0("strtWgt ~ ", stratumVar)), edf, function(x) { sum(x)})
          uhi <- merge(uhi, resw, by=c(stratumVar), all=TRUE)
        } else{
          uhi <- merge(uhi,resi, by=c(psuVar, stratumVar), all=TRUE)
        }
        uhi <- merge(uhi, resj, by=c(stratumVar), all=TRUE)
        uhi[,paste0("dx",bi)] <- uhi[,coln] - uhi[,paste0("uh_",coln)]
      } # End of for loop: bi in 1:length(b)
      # this will be the variance-covariance matrix for this plausible value
      vv <- matrix(0,nrow=length(b), ncol=length(b))
      dofNum <- rep(0,length(b))
      dofDenom <- rep(0,length(b))
      warnDrop <- FALSE
      sa <- lapply(unique(uhi[,stratumVar]), function(ii) {
        vvj <- matrix(0,nrow=length(b), ncol=length(b))
        unkj <- unique(uhi[uhi[,stratumVar] == ii, psuVar, drop=TRUE])
        if(length(unkj)>1) { # cannot estimate variance of single PSU
          sb <- lapply(unkj, function(jj) {
            v <- as.numeric(t(uhi[uhi[,stratumVar]==ii & uhi[,psuVar]==jj, paste0("dx",1:length(b)), drop=FALSE]))
            vvj <<- vvj + v %*% t(v)
          })
          vvj <- vvj * ( (length(unkj)) / ( length(unkj) - 1) )
          num <- diag(D %*% vvj %*% D)
          dofNum <<- dofNum + num
          dofDenom <<- dofDenom + num^2
          vv <<- vv + vvj
        } else { # End of if statment: if(length(unkj)>1)
          warnDrop <<- TRUE
        }
      }) # End of lapply loop: lapply(unique(uhi$repgrp1), function(ii)
      if(warnDrop) {
        warning("Some strata had only one populated PSU. Units in these strata assumed to be certainties. You can condense the strata/PSU structure to avoid this.")
      }
      M <- vv
      vc <- D %*% M %*% D
      # get R-squared
      meanY <- sum(W %*% Y) / sum(diag(W))
      dY <- Y-meanY
      syy <- t(dY) %*% W %*% dY # Weisberg and adding in weights
      # all cases not, not just those in complete strata
      fitted <- X%*%b
      e <- as.vector((Y-fitted))
      rss <- t(e) %*% W %*% e # Weisberg, page 81 eq 4.2
      r2 <- as.numeric(1-(rss/syy)) # Weisberg, page 49 eq 2.31
      coef <- as.numeric(b)
      Vjrr <- as.numeric(diag(vc))
      M <- 1
      Vimp <- 0
      varm <- NULL
      coefm <- NULL
      # this is the VC matrix, store it for vcov to recover
      Ubar <- as.matrix(vc)
    } # end else for if(varMethod == "j")
  } # end else for if(pvy)
  
  # 7) form output, including final variance estimation
  
  # for estimates with no sampling variance, make sure they return NA for SE
  Vjrr[Vjrr == 0] <- NA
  
  V <- Vjrr + Vimp
  names(coef) <- names(coef(lm0))
  se <- sqrt(V)
  names(se) <- names(coef)

  # get fitted and resid based on overall coef
  fitted1 <- as.vector(X%*%coef)
  Y <- sapply(1:length(yvars), function(yi) {
    as.vector(edf[,yvars[yi]])
  }, simplify=TRUE)
  resid1 <- Y - fitted1
  colnames(resid1) <- NULL
  
  # residual df calculation
  nobs <- nrow(edf)
  n.ok <- nobs - sum(edf$origwt==0)
  nvars <- ncol(X)
  rank <- rankMatrix(X, method="qr")[1]
  resdf <- n.ok - rank
  df.r <- resdf
  
  # get residual based on coef by PV
  if(!is.null(coefm)) {
    if(!is.matrix(coefm)) {
      coefm <- t(as.matrix(coefm))
    }
    fitted2 <- as.matrix(X%*%t(coefm))
    resid2 <- Y - fitted2
  }

  coefmat <- data.frame(coef=coef,
                        se=se,
                        t=coef/se)
  
  #assign var that are almost 0 to 0 to make DOF correction run correctly
  if (!is.null(varEstInputs$JK)){
    varEstInputs$JK$value[which(abs(varEstInputs$JK$value) < (sqrt(.Machine$double.eps)*sqrt(nrow(varEstInputs$JK))))] <- 0 
  }
  if(varMethod=="t") {
    m <- length(wgtl$jksuffixes)
    # Johnson and Rust dfo correction
    if(inherits(dofNum, "matrix")) {
      coefmat$dof <- (3.16 - 2.77/sqrt(m)) * apply(dofNum^2/dofDenom, 1, mean)
    } else {
      coefmat$dof <- (3.16 - 2.77/sqrt(m)) * dofNum^2/dofDenom
    }
  } else {
    coefmat$dof <- sapply(names(coef), function(cn) {
      DoFCorrection(varEstA=varEstInputs, varA=cn, method="JR")
    })
  }
  pti <- pt(coefmat$t, df=coefmat$dof)
  coefmat[,"Pr(>|t|)"] <- 2*pmin(pti, 1-pti)
  njk <- length(wgtl$jksuffixes)
  if(varMethod== "t") {
    njk <- NA
  }
  varmeth <- ifelse(varMethod=="t", "Taylor series", "jackknife")
  res <- list(call=call, formula=formula, coef=coef, se=se, Vimp=Vimp,
              Vjrr=Vjrr, M=M, varm=varm, coefm=coefm,
              coefmat=coefmat, r.squared=r2, weight=wgt,
              npv=length(yvars), jrrIMax=min(jrrIMax,length(yvars)),
              njk=njk, varMethod=varmeth,
              residuals=resid1, fitted.values=fitted1, residual.df = resdf)
  if(standardizeWithSamplingVar) {
    coefmatStd <- coefmat
    coefmatStd$coef <- standardizeCoef(coef, std)
    coefmatStd$se <- sqrt(VS)
    res <- c(res, list(coefmatStd=coefmatStd))
  }
  if(!is.null(coefm)) {
    res <- c(res, list(PV.residuals=resid2, PV.fitted.values=fitted2))
  }

  if(returnVarEstInputs) {
    res <- c(res, list(varEstInputs=varEstInputs))
    if(varMethod=="t") {
      warning(paste0("Taylor series method not supported with the ",
              sQuote("varEstInputs"), " argument set to ", dQuote("TRUE"), "."))
    }
  }
  
  if(madeB) {
    # equation 2.29 in var Buuren, pp 42
    tryCatch(
      rbar <- (1+1/M)*(1/nrow(Ubar))*sum(diag(B %*% solve(Ubar))),
      error = function(e){
        rbar <<- (1+1/M)*(1/nrow(Ubar))*sum(diag(B %*% MASS::ginv(Ubar)))
        frm <- Formula(frm)
        if(terms(frm, lhs = 0, rhs = NULL) == ~1){
          warning("A variance estimate was replaced with NA because there was no variance across strata.", call. = FALSE)
        } else {
          warning("Variance estimation problematic; consider using jackknife.", call. = FALSE)
        }
      }
    )
    Ttilde <- (1+rbar)*Ubar
    res <- c(res, list(B=B, U=Ubar, rbar=rbar, Ttilde=Ttilde))
  } else {
    # used for TS vcov
    res <- c(res, list(U=Ubar))
    if(!any(pvy)) {
      res <- c(res, list(B=0*Ubar))
    }
  }
  if(returnNumberOfPSU) {
    res <- c(res, list(nPSU=nrow(unique(edf[,c(stratumVar, psuVar)]))))
  }
  if(all(c(stratumVar, psuVar) %in% colnames(edf))) {
    res <- c(res, list(waldDenomBaseDof=waldDof(edf, stratumVar, psuVar)))
  }
  res <- c(res, list(n0=nrow2.edsurvey.data.frame(data), nUsed=nrow(edf)))
  if(inherits(data, "edsurvey.data.frame")) {
    res <- c(res, list(data=data))
  } else {
    res <- c(res, list(lm0=lm0))
  }
  res <- c(res, list(Xstdev=std, varSummary=varSummary))
  class(res) <- "edsurveyLm"
  return(res)
}

#' @method print edsurveyLm
#' @export
print.edsurveyLm <- function(x, ...) {
  print(coef(x), ...)
}

#' @method print edsurveyLmList
#' @export
print.edsurveyLmList <- function(x, ...) {
  for(i in 1:length(x)) {
    cat("lm", i, "\n")
    print(coef(x[[i]]), ...)
  }
}


# @param src a logical indicating if the the standardized regression coefficients
#            should be included in the coefficients table
#' @method summary edsurveyLm
#' @export
summary.edsurveyLm <- function(object, src=FALSE, ...) {
  class(object) <- "summary.edsurveyLm"
  if(src) {
    if("coefmatStd" %in% names(object)) {
      cm <- object$coefmat
      cm$stdCoef <- object$coefmatStd$coef
      cm$stdSE <- object$coefmatStd$se
    } else {
      cm <- object$coefmat
      std <- object$Xstdev
      cm$stdCoef <- NA
      cm$stdSE <- NA
      for(i in 1:nrow(cm)) {
        if(rownames(cm)[i] %in% names(std) && std[rownames(cm)[i]] > 0) {
          # standardize regression coef = sdX_i/sdY * coef_i 
          cm$stdCoef[i] <- (std[[rownames(cm)[i]]]/std[["outcome.std"]]) * cm$coef[i]
          # same formula for standareized SE
          cm$stdSE[i] <- (std[[rownames(cm)[i]]]/std[["outcome.std"]]) * cm$se[i]
        }
      }
    } # end else for if("coefmatStd" %in% names(object))
    object$coefmat <- cm
  } # end if(src)
  return(object)
}

#' @method summary edsurveyLmList
#' @export
summary.edsurveyLmList <- function(object, smd=FALSE, ...) {
  class(object) <- "summary.edsurveyLmList"
  for(i in 1:length(object)) {
    object[[i]] <- summary.edsurveyLm(object[[i]], smd)
  }
  object
}

#' @method print summary.edsurveyLm
#' @importFrom stats printCoefmat
#' @export
print.summary.edsurveyLm <- function(x, ...) {
  cat(paste0("\nFormula: ", paste(deparse(x$formula), collapse=""),"\n\n"))
  cat(paste0("Weight variable: ", sQuote(x$weight), "\n"))
  cat(paste0("Variance method: ",x$varMethod,"\n"))
  if(!is.na(x$njk)) {
    cat(paste0("JK replicates: ", x$njk, "\n"))
  }
  if(x$npv != 1) {
    cat(paste0("Plausible values: ", x$npv, "\n"))
    cat(paste0("jrrIMax: ", x$jrrIMax, "\n"))
  }
  cat(paste0("full data n: ", x$n0, "\n"))
  if (!is.null(x$nPSU)) {
    cat(paste0("n PSU: ", x$nPSU, "\n"))
  }
  cat(paste0("n used: ", x$nUsed, "\n\n"))
  
  cat(paste0("Coefficients:\n"))
  csind <- which(colnames(x$coefmat) %in% c("coef", "se", "stdCoef", "stdSE"))
  printCoefmat(x$coefmat, P.values=TRUE, has.Pvalue=TRUE, cs.ind=csind)
  cat("\n")
  cat(paste0("Multiple R-squared: ", round(x$r.squared,4), "\n\n"))
}

#' @method print summary.edsurveyLmList
#' @export
print.summary.edsurveyLmList <- function(x, ...) {
  for(i in 1:length(x)) {
    cat("lm", i, "\n")
    print(x[[i]], ...)
  }
}

#' @method coef edsurveyLm
#' @export
coef.edsurveyLm <- function(object, ...) {
  object$coef
}

#' @method coef edsurveyLmList
#' @export
coef.edsurveyLmList <- function(object, ...) {
  sapply(object, function(li) {
    li$coef
  })
}
 
#' @method vcov edsurveyLm
#' @export
vcov.edsurveyLm <- function(object, ...) {
  if(all(c("U", "B") %in% names(object))) {
    if(object$M > 1) {
      # there are PVs, V_samp + V_imp
      return(object$U + (object$M + 1)/object$M * object$B)
    } else {
      # no PVs, V_samp only
      return(object$U)
    }
  }
  if(is.null(object$varEstInputs)){
    stop("This model must be fit with returnVarEstInputs=TRUE or with Taylor series to find the covariance matrix.")
  }
  varnames <- expand.grid(names(coef(object)),names(coef(object)))
  vc <- mapply(varEstToCov, varA = varnames$Var1, varB = varnames$Var2, MoreArgs = list(varEstA = object$varEstInputs, jkSumMultiplier = object$data$jkSumMultiplier))
  matrix(vc, nrow = length(coef(object)), ncol = length(coef(object)))
}

# @export
setMethod("lm",
          c(data="edsurvey.data.frame"),
          lm.sdf)

# @export
setMethod("lm",
          c(data="edsurvey.data.frame.list"),
          lm.sdf)

# @export
setMethod("coef",
          c(object="edsurveyLm"),
          coef.edsurveyLm)

# @export
setMethod("coef",
          c(object="edsurveyLmList"),
          coef.edsurveyLmList)

#' @method plot edsurveyLm
#' @export
plot.edsurveyLm <- function(x, ...) {
  if("lm0" %in% names(x)) {
    cr <- x$lm0
  } else {
    # reevaluate to get lm0
    cl <- x$call
    cl$returnLm0 <- TRUE
    cl[[1]] <- quote(calc.lm.sdf)
    cr <- eval(cl)
  }
  plot(cr)
}

# helper to get standard deviations for a weighted variable
getStdev <- function(outcomeData, XData, weightData) {
  std <- c(outcome=fast.sd(outcomeData, weightData)["std"])
  for(i in 1:ncol(XData)) {
    if(!is.na(sd(XData[,i])) & sd(XData[,i]) > 0) {
      std <- c(std, fast.sd(XData[,i], weightData)["std"])
    } else {
      std <- c(std, 0)
    }
    names(std) <- c(names(std)[-length(std)], colnames(XData)[i])
  }
  return(std)
}

# coef is the (named) coefficients)
# std is the (named) standard deviations
standardizeCoef <- function(coef, std) {
  if(length(names(coef)) == 0) {
    stop("coef must have names.")
  }
  for(si in 1:length(coef)) {
    coefName <- names(coef)[si]
    if(std[[coefName]] > 0) {
      # standardize regression coef = sdX_i/sdY * coef_i 
      coef[si] <- (std[[coefName]]/std[["outcome.std"]]) * coef[si]
    } else {
      coef[si] <- NA
    }
  }
  return(coef)
}
