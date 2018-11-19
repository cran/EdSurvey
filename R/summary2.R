#' @title Summarize edsurvey.data.frame Variables
#'
#' @description Summarizes edsurvey.data.frame variables.
#'
#' @param data an \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}
#' @param variable character variable name
#' @param weightVar character weight variable name. Default to be the default weight of \code{data} if exists.
#'                  If the given survey data does not have a default weight, the function will produce unweighted statistics instead. 
#'                  Can be set to \code{NULL} to return unweighted statistics.
#' @param omittedLevels a logical value. When set to \code{TRUE}, drops those levels of the specified \code{variable}.
#'                     Use print on an \code{edsurvey.data.frame} to see the omitted levels. Defaults to \code{FALSE}.
#'
#' @return 
#' summary of weighted or unweighted statistics of a given variable in an \code{edsurvey.data.frame}  
#' 
#' For categorical variables, summary results include:
#' \describe{
#'   \item{N}{number of cases for each category. Weighted N is also produced if users choose to produce weighted statistics.}
#'   \item{Percent}{percentage of each category. Weighted Percent is also produced if users choose to produce weighted statistics.}
#'   \item{SE}{standard error of the percentage statistics}
#'  }
#'  
#' For continuous variables, summary results include:
#' \describe{
#'   \item{N}{total number of cases (both valid and invalid cases)}
#'   \item{Min.}{smallest value of the variable}
#'   \item{1st Qu.}{first quantile of the variable}
#'   \item{Median}{median value of the variable}
#'   \item{Mean}{mean of the variable}
#'   \item{3rd Qu.}{third quantile of the variable}
#'   \item{Max.}{largest value of the variable}
#'   \item{SD}{standard deviation or weighted standard deviation}
#'   \item{NA's}{number of NA's in variable and in weight variables}
#'   \item{Zero-weight's}{number of zero-weight cases if users choose to produce weighted statistics}
#' }
#' 
#' If the weight option is chosen, the function produces weighted percentile and standard deviation. Refer to the vignette titled 
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistics} and
#' the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Percentiles.pdf}{Percentile}
#' for how the function calculates these statistics (with and without plausible values). 
#' 
#' @importFrom stats quantile
#' @export
#' @example man\examples\summary2.R
#' @seealso \code{\link{percentile}} 
#' @author Trang Nguyen
summary2 <- function(data,variable,
                     weightVar=attr(getAttributes(data,"weights"),"default"),
                     omittedLevels=FALSE) {
  checkDataClass(data,c("light.edsurvey.data.frame","edsurvey.data.frame"))
  callc <- match.call()
  if (is.null(weightVar) || !weightVar %in% colnames(data)) {
    callc$weightVar <- NULL
    edf <- getData(data,variable, omittedLevels = omittedLevels, includeNaLabel = !omittedLevels)
    N <- nrow(edf)
    if (typeOfVariable(variable,data) == "discrete") {
      ret <- as.data.frame(ftable(edf[,variable], exclude = NULL))
      colnames(ret) <- c(variable, "N")
      ret$Percent <- ret$N/N * 100
      ret <- list(summary=ret)
      ret$call <- callc
      class(ret) <- "summary2"
      return(ret)
    } else {
      ret <- lapply(1:ncol(edf), function(i) {
        descriptiveContinuous(edf[[i]])
      })
      ret <- cbind("Variable" = names(edf), as.data.frame(do.call('rbind',ret)))
      ret <- list(summary=ret)
      ret$call <- callc
      class(ret) <- "summary2"
      return(ret)
    }
  } # end if(is.null(weightVar))
  
  callc$weightVar <- weightVar
  data <- getData(data,c(variable, weightVar), omittedLevels = omittedLevels, addAttributes = TRUE, includeNaLabel = !omittedLevels, drop = FALSE)
  if(typeOfVariable(variable,data) == "discrete") {
    ret <- edsurveyTable(as.formula(paste0("~ ",variable)),
                         data=data,returnMeans=FALSE,
                         omittedLevels = omittedLevels,
                         weightVar = weightVar)
    colnames(ret$data)[3] <- "Weighted N"
    colnames(ret$data)[4] <- "Weighted Percent"
    colnames(ret$data)[5] <- "Weighted Percent SE"
    ret <- ret$data[,1:5]
  } else {
    variable_r <- variable
    ret <- as.data.frame(percentile(variable,data=data,percentiles=c(0,25,50,75,100), weightVar = weightVar))$estimate
    if (hasPlausibleValue(variable,data)) {
      variable <- getPlausibleValue(variable,data)
    }
    lm0 <- fast.sd(data[,variable], data[,weightVar])
    mean_var <- lm0$mean
    sd_var <- lm0$std
    n <- nrow(data)
    wN <- sum(data[,weightVar],na.rm = TRUE)
    nNA <- sum(rowSums(is.na(data[,variable,drop=FALSE])) > 0 | is.na(data[,weightVar]))
    ret <- c(n,wN,ret[1:3],mean_var,ret[4:5],sd_var,nNA, sum(data[,weightVar] == 0, na.rm = TRUE))
    names(ret) <- c("N","Weighted N","Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", 
                    "Max.", "SD","NA's", "Zero-weight's")
    ret <- cbind("Variable" = variable_r, as.data.frame(do.call('rbind',list(ret))))
  }
  ret <- list(summary=ret)
  ret$call <- callc
  class(ret) <- "summary2"
  return(ret)
}



#' @method print summary2
#' @export
print.summary2 <- function(x, ...) {
  call <- x$call
  if (!"weightVar" %in% names(call)) {
    cat("Estimates are not weighted.\n")
  } else {
    cat(paste0("Estimates are weighted using weight variable ", sQuote(call$weightVar),"\n"))
  }
  print(x$summary, ...)
}



# calculates a mean and standard deviation (std) estimate based on variables
# tha are already read in.
# variables: the variables in this variable (this is not vectorized, simply allows for PV vars)
# weight: the full sample weight
fast.sd <- function(variables, weight) { 
  y <- as.matrix(variables) # need to abstract PVs
  se <- mu <- rep(NA, ncol(y))
  for(i in 1:ncol(y)) {
    y0 <- y[!is.na(y[,i]) & !is.na(weight) & weight != 0,i]
    w0 <- weight[!is.na(y[,i]) & !is.na(weight) & weight !=0]
    mu[i] <- sum(w0 * y0)/sum(w0)
    se[i] <- sum(w0 * (y0 - mu[i])^2)/sum(w0)
  }
  return(list(mean=mean(mu), std=sqrt(mean(se))))
}

# returns the N, min, quartiles, max, mean, and sd of x, and number of NA's
# does no checking of x
descriptiveContinuous <- function(x) {
  q <- quantile(x, probs = c(0,0.25,0.5,0.75,1), na.rm = TRUE, type = 8)
  ret <- c(length(x),q[1:3],mean(x,na.rm=TRUE),q[4:5],sd(x,na.rm = TRUE), sum(is.na(x)))
  names(ret) <- c("N","Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", 
                  "Max.", "SD", "NA's")

  return(ret)
}
