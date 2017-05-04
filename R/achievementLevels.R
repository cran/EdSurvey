#' @title Return achievement levels for an edsurvey.data.frame.
#'
#' @description Returns achievement levels using weights and variance estimates appropriate for the \code{edsurvey.data.frame}.
#'
#' @param achievementVars character vector indicating variables to be included in the achievement 
#'                        levels table, potentially with a subject scale or subscale. When the subject 
#'                        scale or subscale is omitted, then the default subject scale or subscale is 
#'                        used. You can find the default composite scale and all subscales using the 
#'                        function \code{\link{showPlausibleValues}}.
#' @param aggregateBy character vector specifying variables to aggregate achievement levels by. The percent
#'                    column sums up to 100 for all levels of all variables specified here. When set to 
#'                    default of \code{NULL}, the percent column sums up to 100 for all levels of all variables specified in 
#'                    in \code{achievementVars}.
#' @param data      an edsurvey.data.frame.
#' @param weightVar character indicating the weight variable to use; see Details.
#' @param cutpoints numeric vector indicating cut points. Set to standard NAEP cut points for 
#'                  Basic, Proficient, Advanced by default.
#' @param jrrIMax  numeric value. When using jackknife variance estimation method, the \eqn{V_{jrr}} term
#'                 (see Details) can be estimated with any positive number of plausible values and is 
#'                 estimated on the first of the lower of the number of available plausible values and 
#'                 \code{jrrIMax}. When \code{jrrIMax} is set to Inf, all of the plausible values will 
#'                 be used. Higher values of \code{jrrIMax} lead to longer computing times and more
#'                 accurate variance estimates.
#' @param schoolMergeVarStudent a character variable name from the student file used to merge student 
#'                              and school data files. Set to \code{NULL} by default.
#' @param schoolMergeVarSchool a character variable name from the school file used to merge student 
#'                             and school data files. Set to \code{NULL} by default.
#' @param omittedLevels a logical value. When set to the default value of \code{TRUE}, drops those levels of 
#'                      all factor variables that are specified in \code{achievementVars} and \code{aggregateBy}. 
#'                      Use \code{print} on an \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses the default 
#'                          conditions stored in \code{edsurvey.data.frame} to subset the data. 
#'                          Use \code{print} on an \code{edsurvey.data.frame} to see the default
#'                          conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'               recode = list(var1= list(from=c("a,"b","c"), to ="d")). See Examples.
#' @param returnDiscrete logical indicating if discrete achievement levels should be returned. Defaults 
#'                       to \code{TRUE}.
#' @param returnCumulative logical indicating if cumulative achievement levels should be returned. Defaults
#'                         to \code{FALSE}.
#' @author Ahmad Emad
#' @details The \code{achievementLevels} function applies appropriate weights and variance estimation method for each
#'          \code{edsurvey.data.frame}, with several arguments for customizing the aggregation and output of the analysis 
#'          results. Namely, by using these optional arguments, users can choose to generate the percentage of students 
#'          performing at each achievement level (discrete), at or above each achievement level (cumulative), 
#'          calculating the percentage distribution of students by achievement levels (discrete or cumulative) and 
#'          selected characteristics (specified in \code{aggregateBy}), and computing the percentage distribution of students 
#'          by selected characteristics within a specific achievement level.
#'
#' \subsection{Calculation of percentages}{
#'          The details of the methods are shown in the statistics vignette, which you can read by 
#'          running \code{vignette("statistics", package="EdSurvey")} at the R prompt. The methods described in
#'          \dQuote{Estimation of weighted percentages when plausible values are present} are use to calculate 
#'          all cumulative and discrete probabilities.
#'
#'          When the requested achievement levels are discrete (\code{returnDiscrete = TRUE}),
#'          the percentage \eqn{\mathcal{A}} is the percentage of students (within the categories specified in \code{aggregateBy}) 
#'          whose scores lie in the range  [\eqn{cutPoints_i}, \eqn{cutPoints_{i+1}}), i = 0,1,...,n.
#'          \code{cutPoints} is the score thresholds provided by the user with \eqn{cutPoints_0} taken
#'          to be 0. \code{cutPoints} are set to NAEP standard cut points for achievement levels by default.
#'          To aggregate by a specific variable, for example, \code{dsex}, specify \code{dsex} in \code{aggregateBy}
#'          and all other variables in \code{achievementVars}. To aggregate by achievement levels, specify 
#'          the name of the plausible value in \code{aggergateBy} and all other variables in 
#'          \code{achievementVars} 
#'          
#'
#'          When the requested achievement levels are cumulative (\code{returnCumulative = TRUE})
#'          the percentage \eqn{\mathcal{A}} is the percentage of students (within the categories specified in \dQuote{aggregateBy}) 
#'          whose scores lie in the range  [\eqn{cutPoints_i}, \eqn{\infty}), i = 1,2...,n-1. The 
#'          first and last categories are the same as defined for discrete levels.
#' } 
#'         
#' \subsection{Calculation of standard error of percentages}{
#'          The method used to calculate the standard error of the percentages is described in the \emph{Statistics} vignette
#'          in the section
#' \dQuote{Estimation of the standard error of weighted percentages when plausible values are present, using the jackknife method.}
#'         the value of \code{jrrIMax} sets the value of \eqn{m^*}.
#' }
#'          
#' @return
#' A \code{list} containing up to two \code{data.frame} (s), one for each of the discrete and cumulative achievement levels
#' as determined by \code{returnDiscrete} and \code{returnCumulative}. The \code{data.frame} contains the following columns:
#' \item{\code{Level}}{One row for each level of the specified achievement cut points.}
#' \item{Variables in achievementVars}{One column for each variable in \code{achievementVars}, and one row for each level of each variable in \code{achievementVars}.}
#' \item{\code{Percent}}{Percentage of students at or above each achievement level aggregated as specified by \code{aggregateBy}.}
#' \item{\code{StandardError}}{The standard error of the percentage, accounting for the survey sampling methodology. See the statistics vignette.}
#' \item{\code{n0}}{The number of observations in the incomding data (the
#'                  number of rows when \code{omittedLevels} and
#'                  \code{defaultConditions} are set to \code{FALSE}.}
#' \item{\code{nUsed}}{The number of observations in the data after applying all
#'                     filters (see \code{omittedLevels} and
#'                     \code{defaultConditions}).}
#' @references
#' Rubin, D. B. (1987). \emph{Multiple Imputation for Nonresponse in Surveys}. New York, NY: Wiley.
#'
#' @importFrom data.table setDT copy
#' @importFrom stats as.formula terms
#' @example man/examples/achievementLevels.R
#' @export

achievementLevels <- function(achievementVars = NULL, 
                              aggregateBy = NULL,
                              data,
                              cutpoints = NULL,
                              returnDiscrete = TRUE,
                              returnCumulative = FALSE,
                              weightVar=NULL,
                              jrrIMax=1,
                              schoolMergeVarStudent=NULL,
                              schoolMergeVarSchool=NULL,
                              omittedLevels=TRUE,
                              defaultConditions=TRUE,
                              recode = NULL
) {
  
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  res2 <- list()
  if(inherits(data, "edsurvey.data.frame.list")) {
    ll <- length(data$datalist)
    labels <- as.character(data$covs$labels)
    #print(labels)
    if(is.null(labels)) {
      labels <- as.character(c(1:ll))
    }
    
    for(i in 1:ll) {
      sdf <- data$datalist[[i]]
      cat("Working on dataset", labels[i], "\n")
      # result <- calcAchievement(achievementVars, aggregateBy, sdf, cutpoints, returnDiscrete,
      #                           returnCumulative, weightVar, jrrIMax, schoolMergeVarSchool, 
      #                           schoolMergeVarStudent, omittedLevels, defaultConditions,
      #                           recode)
      # print(result)
      
      temp <- tryCatch(result <- calcAchievement(achievementVars, aggregateBy, sdf, cutpoints, returnDiscrete,
                                                 returnCumulative, weightVar, jrrIMax, schoolMergeVarSchool,
                                                 schoolMergeVarStudent, omittedLevels, defaultConditions,
                                                 recode),
                       error = function(cond) {
                         message(paste("Error occurred while working on dataset", labels[i]))
                         message(cond)
                         return(0)
                       },
                       warning= function(cond) {
                         message(paste("Warning occurred while working on dataset", labels[i]))
                         message(cond)
                         return(NA)
                       }
      )
      if(class(temp) == "achievementLevels") {
        res2[[labels[i]]] <- result
      }
    }
    class(res2) <- "achievementLevels"
    res2
  } 
  else {
    result <- calcAchievement(achievementVars, aggregateBy, data, cutpoints, returnDiscrete,
                              returnCumulative, weightVar, jrrIMax, schoolMergeVarSchool, 
                              schoolMergeVarStudent, omittedLevels, defaultConditions,
                              recode, defaultConditionsMissing=missing(defaultConditions))
    result
  }
  
}

calcAchievement <- function(achievementVars=NULL, 
                            aggregateBy=NULL,
                            data,
                            cutpoints=NULL,
                            returnDiscrete=TRUE,
                            returnCumulative=FALSE,
                            weightVar=NULL,
                            jrrIMax=1,
                            schoolMergeVarStudent=NULL,
                            schoolMergeVarSchool=NULL,
                            omittedLevels=TRUE,
                            defaultConditions=TRUE,
                            recode=NULL,
                            defaultConditionsMissing=FALSE) {
  
  # This bit of code checks that the arguments supplied are correct. Standard boilerplate
  sdf <- data
  als <- getAttributes(sdf, "achievementLevels")
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  if(als[1] == "Not Found" & is.null(cutpoints)) {
    stop(paste0("Default achievement levels not found. ", sQuote("cutpoints"), " must be set."))
  }
  
  # Determine if the user supplied variables for calculating achievementlevels, 
  # otherwise just use the default plausible value
  if(is.null(achievementVars)) {
    achievementVars <- attributes(getAttributes(sdf, "pvvars"))$default
  }
  # Check to see only one variable in all the supplied variables has plausible values
  vars <- unique(c(achievementVars, aggregateBy))
  has.more.pvs <- sum(sapply(vars, FUN= function(x) hasPlausibleValue(x, sdf)))
  if(has.more.pvs > 1) {
    stop(paste0("More than one variable with associated plausible values found in the ",
         sQuote("achievementVars"), " or ", sQuote("aggregateBy"), " arguments."))
   }
  
  # Determine if weight supplied, otherwise use default weight
  if(is.null(weightVar)) {
    wgt <- attributes(getAttributes(sdf, "weights"))$default
  } else {
    wgt <- weightVar
  }
  
  # Get yvar, if no yvar is specified 
  has.pv <- sum(sapply(vars, FUN= function(x) hasPlausibleValue(x, sdf)))
  
  if(has.pv == 0) {
    achievementVars <- attributes(getAttributes(sdf, "pvvars"))$default
  } else {
    yvar <- vars[sapply(vars, FUN= function(x) hasPlausibleValue(x, sdf))]
  }
  
  # Check to see if at least one plausible value variable identified
  if(length(yvar) == 0) {
    stop(paste0("At least one variable in ", sQuote("achievementVars"), " must have plausible values to calculate achievement levels."))
  }
  
  pvs <- getPlausibleValue(yvar, sdf)
  taylorVars <- c() # no Taylor series, so no Taylor vars
  # Once we add taylor series, this will need to change.
  varMethod <- "j" # use jackknife for now
  # get the data. This is most of the arguments
  getDataArgs <- list(data=sdf,
                      varnames=c(vars, wgt, taylorVars),
                      returnJKreplicates=(varMethod=="j"),
                      drop=FALSE,
                      schoolMergeVarStudent=schoolMergeVarStudent,
                      schoolMergeVarSchool=schoolMergeVarSchool,
                      omittedLevels=omittedLevels,
                      recode=recode,
                      includeNaLabel=TRUE,
                      dropUnusedLevels=TRUE)
  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if(!defaultConditionsMissing) {
    getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
  }
  edf <- do.call(getData, getDataArgs)
  # check if there is any data
  if(nrow(edf) <= 0) {
    stop(paste0(sQuote("data"), " must have more than 0 rows after a call to ", sQuote("getData"), "."))
  }
  
  if(length(vars[!vars==yvar]) == 0) {
    vars <- c(yvar, "1")
  }
  # Take the plausible value identified and all the aggregating variables
  # and create a formula
  formula <- as.formula(paste(yvar, paste(vars[!vars==yvar], collapse = "+"), sep="~"))
  
  # Determine if the user supplied cutpoints.
  # If the user did not supply cutpoints, get the default ones stored in sdf
  ALattr <- getAttributes(sdf, "achievementLevels")
  temp <- as.numeric(ALattr)
  names(temp) <- names(ALattr)
  #temp <- c(as.numeric(sdf$achievementLevels[1]), as.numeric(sdf$achievementLevels[2]), as.numeric(sdf$achievementLevels[3]))
  if(is.null(cutpoints)) {
    nCutpoints <- temp
    nCutpoints <- rep(nCutpoints, nrow(edf)/length(nCutpoints))[1:nrow(edf)]
  } else {
    nCutpoints <- rep(cutpoints, nrow(edf)/length(cutpoints))[1:nrow(edf)]
  }
  # Add the cutpoints to the data.frame
  edf$cuts <- nCutpoints
  
  jkWeights <- getWeightJkReplicates(wgt, sdf)
  
  vm <- ifelse(varMethod=="j", "jackknife", "Taylor series")
  # Gather information about the call.
  callVars <- list(achievementVars=achievementVars,
                   aggregateBy=aggregateBy,
                   cutpoints=sort(unique(nCutpoints)),
                   weightVar=wgt,
                   jrrIMax=min(length(pvs), jrrIMax),
                   npv=length(pvs),
                   varMethod=vm,
                   njk=length(jkWeights))
  
  sortnames <- c(aggregateBy[aggregateBy!=yvar], "Level")
  
  
  # Call the appropriate functions to get the appropriate results
  # add the discrete, if requested
  if(returnDiscrete) {
    #discreteFunc returns a  data.frame containing the discrete achievement levels
    discrete1 <- discreteFunc(pvs, edf, formula ,aggregateBy, wgt, jrrIMax, jkWeights, varMethod, length(pvs))
    discrete1 <- discrete1[do.call("order", discrete1[sortnames]), ]
    discrete1 <- addLabels(discrete1, cutpoints, "discrete", temp)
  }
  
  # add the cumulative, if requested
  if(returnCumulative) {
    #cumulativeFunc returns a data.frame containing cumulative achievement levels.
    cumulative1 <- cumulativeFunc(pvs, edf, formula ,aggregateBy, wgt, jrrIMax, jkWeights, varMethod, length(pvs))
    cumulative1 <- cumulative1[do.call("order", cumulative1[sortnames]), ]
    cumulative1 <- addLabels(cumulative1, cutpoints, "cumulative", temp)
  }
  
  # Create a list containing the achievement levels and the call information
  res <- list(callVars=callVars)
  if(returnDiscrete & returnCumulative) {
  }
  if(returnDiscrete) {
    res <- c(res, list(discrete=discrete1))
  }
  if(returnCumulative) {
    res <- c(res, list(cumulative=cumulative1))
  }
  res <- c(res, list(n0=nrow2.edsurvey.data.frame(data),nUsed=nrow(edf)))
  class(res) <- "achievementLevels"
  res
}




discreteFunc <- function(pvs, edf, formula, aggregateBy, weight, jrrIMax, jkWeights, varMethod, nPv) {
  #discreteFunc returns a  data.frame containing the discrete achievement levels
  rhsVars <- labels(terms(formula))
  # Run the attachLevels function for each of the plausible value variables
  piem <- sapply(pvs, FUN = function(x) {attachLevels(x, edf, formula,aggregateBy, weight, "discrete", includeN=TRUE)}, USE.NAMES=TRUE, simplify=FALSE)
  # Convert the results to a data.frame
  discrete <- do.call("rbind", piem)
  aggVars <- names(discrete)[!names(discrete) %in% c("Percent", "N", "wtdN")]
  aggVars <- paste(paste(aggVars, collapse="+"), sep="")
  aggFormula <- as.formula(paste("cbind(Percent, N, wtdN) ~", aggVars))
  # Aggregate by summing, and then dividing by the number of plausible values
  discrete <- aggregate(aggFormula, data=discrete, FUN=sum)
  tempVars <- names(discrete)[!names(discrete) %in% labels(terms(formula))]
  tempVars <- tempVars[tempVars!="Level"]
  discrete[, tempVars] <- discrete[, tempVars]/nPv
  
  # Calculate the two variances, and add them together.
  if(varMethod == "j") {
    varMeasurementDiscrete <- varianceMeasurement(pvs, formula, discrete, piem, "discrete")
    varSamplingDiscrete <- varianceSampling(pvs, jrrIMax, jkWeights, edf, formula, piem, aggregateBy, "discrete")
    stdErr <- merge(varMeasurementDiscrete, varSamplingDiscrete, by = names(varSamplingDiscrete)[names(varSamplingDiscrete) != "var"])
    M <- length(pvs)
    stdErr$StandardError <- sqrt(stdErr$var.y + ((M+1)/M) * stdErr$var.x )
    stdErr$var.x <- NULL
    stdErr$var.y <- NULL
    
    result <- merge(discrete, stdErr, by = names(stdErr)[!names(stdErr) %in% c("Percent", "StandardError", "N", "wtdN")], all=TRUE)
  }
  else {
    # not used
    result <- seTaylor(edf, discrete, aggregateBy, formula, weight, pvs, "discrete")
  }
  temp <- names(result)[!names(result) %in% c("Level", "N", "wtdN", "Percent", "StandardError")]
  result  <- result[, c("Level", temp, "N", "wtdN", "Percent", "StandardError")]
  
  return(result)
}

cumulativeFunc <- function(pvs, edf, formula, aggregateBy, weight, jrrIMax, jkWeights, varMethod, nPv) {
  #cumulativeFunc returns a data.frame containing cumulative achievement levels.
  if(varMethod == "t") {
    stop("Only jackknife variance estimation implemented for cumulative.")
  }
  rhsVars <- labels(terms(formula))
  # Run the attachLevels function for each of the plausible value variables
  piem <- sapply(pvs, FUN = function(x) {attachLevels(x, edf, formula,aggregateBy, weight,
                                                      "cumulative", includeN=TRUE)}, USE.NAMES=TRUE, simplify=FALSE)
  # Convert the results to a data.frame
  cumulative <- do.call("rbind", piem)
  aggVars <- names(cumulative)[!names(cumulative) %in% c("Percent", "N", "wtdN")]
  
  aggVars <- paste(paste(aggVars, collapse="+"), sep="")
  aggFormula <- as.formula(paste("cbind(Percent, N, wtdN) ~", aggVars))
  # Aggregate by summing, and then dividing by the number of plausible values
  
  cumulative <- aggregate(aggFormula, data=cumulative, FUN=sum)
  tempVars <- names(cumulative)[!names(cumulative) %in% labels(terms(formula))]
  tempVars <- tempVars[tempVars!="Level"]
  cumulative[, tempVars] <- cumulative[, tempVars]/nPv
  # Calculate the two variances, and add them together.
  
  varMeasurementCumulative <- varianceMeasurement(pvs, formula, cumulative, piem, "cumulative")
  varSamplingCumulative <- varianceSampling(pvs, jrrIMax, jkWeights, edf, formula, piem, aggregateBy, "cumulative")
  
  stdErr <- merge(varMeasurementCumulative, varSamplingCumulative, by = names(varSamplingCumulative)[names(varSamplingCumulative) != "var"])
  M <- length(pvs)
  stdErr$StandardError <- sqrt(stdErr$var.y + ((M+1)/M) * stdErr$var.x )
  stdErr$var.x <- NULL
  stdErr$var.y <- NULL
  result <- merge(cumulative, stdErr, by = names(stdErr)[!names(stdErr) %in% c("Percent", "StandardError", "N", "wtdN")], all=TRUE)
  temp <- names(result)[!names(result) %in% c("Level", "N", "wtdN", "Percent", "StandardError")]
  result  <- result[, c("Level", temp, "N", "wtdN", "Percent", "StandardError")]
  
  return(result)
}

varianceSampling <- function(pvs, jrrIMax, jkWeights, data, formula, piem, aggregateBy, type) {
  # This function calculates the sampling variance. It follows the documentation.
  pvs <- pvs[1:(min(length(pvs), jrrIMax))]
  temp <- sapply(jkWeights, FUN = function(jk) {
    pvJK <- sapply(pvs, function(pv) {
      piemTemp <- piem[[pv]]
      piemTemp <- piemTemp[,!names(piemTemp) %in% c("N", "wtdN")]
      temp <- attachLevels(pv, data, formula, aggregateBy, jk, type)
      temp <- merge(temp, piemTemp,
                    by = names(temp)[!names(temp) %in% c("Percent", "N", "wtdN")],
                    all=TRUE, suffixes=c(".x", ".y"))
      temp$var <- (temp$Percent.x - temp$Percent.y)^2
      temp$Percent.x <- NULL
      temp$Percent.y <- NULL
      temp
    }, simplify=FALSE) # sapply(pvs, function(pv)
    do.call("rbind", pvJK)
  }, simplify = FALSE) # sapply(jkWeights, FUN = function(jk)
  
  temp <- do.call("rbind", temp)
  aggVars <- names(temp)[!names(temp) %in% c("var")]
  aggVars <- paste(paste(aggVars, collapse="+"), sep="")
  aggFormula <- as.formula(paste("var ~", aggVars))
  temp <- aggregate(aggFormula, data=temp, FUN=sum)
  if(length(pvs) >1) {
    temp$var <- temp$var/(length(pvs) - 1)
  }  
  
  temp
}

varianceMeasurement <- function(pvs, formula,discrete, piem, type) {
  # This function calculates the measurement variance.
  discrete <- discrete[,!names(discrete) %in% c("N", "wtdN")]
  temp <- sapply(pvs, FUN = function(pv) {
    piemTemp <- piem[[pv]]
    piemTemp <- piemTemp[,!names(piemTemp) %in% c("N", "wtdN")]
    temp1 <- merge(discrete,piemTemp , by = names(discrete)[!names(discrete) %in% c("Percent", "N", "wtdN")], all=TRUE)
    temp1$var <- (temp1$Percent.x - temp1$Percent.y)^2
    temp1$Percent.x <- NULL
    temp1$Percent.y <- NULL
    temp1
  }, simplify = FALSE)
  
  temp <- do.call("rbind", temp)
  
  aggVars <- names(temp)[!names(temp) %in% c("var")]
  aggVars <- paste(paste(aggVars, collapse="+"), sep="")
  aggFormula <- as.formula(paste("var ~", aggVars))
  
  temp <- aggregate(aggFormula, data=temp, FUN=sum)
  if(length(pvs) >1) {
    temp$var <- temp$var/(length(pvs) - 1)
  }
  temp
}

#' @importFrom stats reshape
attachLevels <- function(m, data, formula, aggregateBy, weight, type="discrete", includeN=FALSE) {
  ## This function works for both discrete and cumulative levels.
  ## It creates achievement levels for one plausible value (the m argument).
  
  # Extract the yvar
  lhsVar <- as.character(formula[[2]])
  # Remove if there is missing data for pv m.
  data <- data[!is.na(data[,m]),]
  rhsVars <- labels(terms(formula))
  # In case there are no RHS aggregating variables, aggregate over everything.
  if(length(rhsVars) == 0) rhsVars <- "1"
  # Makes a call to the addLevels functions, which adds n additional binary variables
  # to the data.frame, indicating whether a particular score is at a particular
  # achievement level
  data <- addLevels(data, m, type)
  # Since we named the indicator variables above achievementLevel1, achievementLevel2,
  # and so on, we can extract them.
  namesLevs <- names(data)[grepl("achievementLevel", names(data))]
  # Check to see if you want to aggregateBy the achievement levels themselves
  if(lhsVar %in% aggregateBy) {
    aggregateBy <- c(namesLevs, aggregateBy)
    aggregateBy <- aggregateBy[aggregateBy != lhsVar]
  }
  # Create the aggregation formula
  aggFormula <- as.formula(paste(weight," ~", paste(rhsVars, collapse=" + "), collapse="" ))
  
  # Create three different lists to store the results for Percent, N and wtdN
  aggData <- list()
  aggDataN <- list()
  aggDataWtdN <- list()
  # Iterate over each achievementLevel and aggregate as specified
  for(i in seq(1, length(namesLevs))) {
    if(nrow(data[data[, namesLevs[i]]==1,]) > 0) {
      aggData[[i]] <- fastAgg(aggFormula, data=data[data[, namesLevs[i]]==1,], FUN=sum)
      aggDataN[[i]] <- fastAgg(aggFormula, data=data[data[, namesLevs[i]]==1,], FUN=length)
    } else {
      aggData[[i]] <- 0 * fastAgg(aggFormula, data=data, FUN=sum)
      aggDataN[[i]] <- 0 * fastAgg(aggFormula, data=data, FUN=length)
    }    
    names(aggData[[i]])[names(aggData[[i]]) == weight] <- namesLevs[i]
    names(aggDataN[[i]])[names(aggDataN[[i]]) == weight] <- namesLevs[i]
  }
  # If there are RHS aggregating variables, merge the three results above
  if(length(labels(terms(formula))) != 0) {
    achLevels <- merge(aggData[1], aggData[2], by=labels(terms(formula)), all=TRUE)
    achN <- merge(aggDataN[1], aggDataN[2], by=labels(terms(formula)), all=TRUE)
    achwtdN <- merge(aggData[1], aggData[2], by=labels(terms(formula)), all=TRUE)
    if(length(namesLevs) >2) {
      for(j in seq(3, length(namesLevs))) {
        achLevels <- merge(achLevels, aggData[j], by=labels(terms(formula)), all=TRUE)
        achN <- merge(achN, aggDataN[j], by=labels(terms(formula)), all=TRUE)
        achwtdN <- merge(achwtdN, aggData[j], by=labels(terms(formula)), all=TRUE)
      }
    }
  }  # if(length(labels(terms(formula))) != 0)
  else {
    achLevels <- do.call("cbind", aggData)
    achN <- do.call("cbind", aggDataN)
    achwtdN <- do.call("cbind", aggData)
    names(achLevels) <- namesLevs
  }
  # reshape to beautify
  achLevels <- reshape(achLevels, varying = namesLevs, 
                       v.names="Value", timevar = m,
                       times=namesLevs,direction = "long", new.row.names = 1:(ncol(achLevels)+1000))
  achLevels <- achLevels[,!names(achLevels) %in% "id"]
  
  # this switch is included here because sometimes you dont care about N
  # especially when you are doing variance estimation
  if(includeN) {
    achN <- reshape(achN, varying = namesLevs, 
                    v.names="N", timevar = m,
                    times=namesLevs,direction = "long", new.row.names = 1:(ncol(achN)+1000))
    achN <- achN[,names(achN) %in% c("N"), drop=FALSE]
    achwtdN <- reshape(achwtdN, varying = namesLevs, 
                       v.names="wtdN", timevar = m,
                       times=namesLevs,direction = "long", new.row.names = 1:(ncol(achwtdN)+1000))
    achwtdN <- achwtdN[,names(achwtdN) %in% c("wtdN"), drop=FALSE]
    achLevels <- cbind(achLevels, achN, achwtdN)
  } # if(includeN)
  
  names(achLevels)[names(achLevels) == m] <- "Level"
  
  # After reshaping, rename the levels appropriately
  data$level <- namesLevs[1]
  for(lev in namesLevs) {
    data$level[data[,lev] == 1] <- lev
  }
  
  # Determine how to aggregate appropriately.
  if(sum(namesLevs %in% aggregateBy) >1) {
    aggregateBy <- aggregateBy[!aggregateBy %in% namesLevs]
    aggregateBy <- c(aggregateBy, "Level")
  }
  
  if(is.null(aggregateBy) | 
     ("Level" %in% aggregateBy & identical(aggregateBy, c(labels(terms(formula)), "Level")))) {
    temp <- sum(data[,weight], na.rm=TRUE)
    achLevels$Percent <- achLevels$Value*100/temp
    achLevels$Value <- NULL
  }
  else {
    if("Level" %in% aggregateBy) {
      aggFormula <- as.formula(paste("Value~", paste(aggregateBy, collapse="+")))
      temp <- aggregate(aggFormula, data=achLevels, FUN=sum)
    }
    else {
      aggFormula <- as.formula(paste(weight,"~", paste(aggregateBy, collapse="+")))
      temp <- aggregate(aggFormula, data=data, FUN=sum)
      names(temp)[names(temp) == weight] <- "Value"
    }
    
    achLevels <- merge(achLevels, temp, by=names(temp)[names(temp) != "Value"], all=TRUE)
    achLevels$Percent <-achLevels$Value.x*100 / achLevels$Value.y
    achLevels$Value.x <- NULL
    achLevels$Value.y <- NULL
  }
  achLevels
}

addLevels <- function(data, m, type) {  
  # This function returns a modified data.frame which has indicator variables added
  # for each of the achievement levels.
  cutpoints <-sort(as.numeric(unique(data$cuts)))
  nlevs <- length(cutpoints)
  for(i in seq(1, nlevs + 1)) {
    temp <- paste("achievementLevel", i, sep="")
    if(type=="discrete") {  
      if(i==1) {
        data[,temp] <- ifelse( data[,m] < cutpoints[i], 1, 0)
      }
      if(i==nlevs + 1) {
        data[,temp] <- ifelse( data[,m] >= cutpoints[i-1], 1, 0)
      }
      if(i > 1 & i <= nlevs) {
        data[,temp] <- ifelse( data[,m] >= cutpoints[i-1] & data[,m] < cutpoints[i], 1, 0)
      }
    }
    else {
      if(i==1) {
        data[,temp] <- ifelse( data[,m] < cutpoints[i], 1, 0)
      }
      else {
        data[,temp] <- ifelse( data[,m] >= cutpoints[i-1], 1, 0)
      }
      
    }
  }
  data
}

addLabels <- function(aLevs, cutpoints, type, temp) {
  # Based on the business rules, this function adds labels to the cutpoints.
  # If the cutpoints are NAEP standard, the NAEP labels are used.
  # Otherwise, a generic range label is returned.
  flag <- FALSE
  if(is.null(cutpoints)) {
    flag <- TRUE
  }
  if(!is.null(cutpoints)) {
    if(identical(temp, cutpoints) & length(temp) == 3) {
      flag <- TRUE
    }
  }
  if(is.null(cutpoints)) {
    cutpoints <- temp
  }

  if(flag) {
    if(type == "discrete") {
      namesLevs <- c(paste0("Below ", names(cutpoints)[1]), paste0("At ",names(cutpoints)))
      #namesLevs <-  c("Below Basic", "At Basic", "At Proficient", "At Advanced")
    }
    else {
      namesLevs <- paste0("Below ", names(cutpoints)[1])
      if(length(cutpoints) > 1) {
       namesLevs <- c(namesLevs, paste0("At or Above ", names(cutpoints)[- length(cutpoints)]))
      }
      namesLevs <- c(namesLevs, paste0("At ", names(cutpoints)[length(cutpoints)]))
      #namesLevs <- c("Below Basic", "At or Above Basic", "At or Above Proficient", "At Advanced")  
    }
    for(i in 1:length(namesLevs)) {
      aLevs$Level[aLevs$Level == paste0("achievementLevel", i)] <- namesLevs[i]
    }
  }
  
  if(!is.null(cutpoints) & flag==FALSE) {
    for(i in seq(1, length(cutpoints) + 1)) {
      tempLabel <- paste0("achievementLevel", i)
      if(i==1) {
        label <- paste("<", cutpoints[i])
      }
      if(i == length(cutpoints) + 1) {
        label <- paste(">=", cutpoints[i-1])
      }
      if(i >1 & i <= length(cutpoints)) {
        if(type=="discrete") {
          label <- paste0("[", cutpoints[i-1], ", ", cutpoints[i], ")")  
        }
        else {
          label <- paste(">=", cutpoints[i-1])
        }
        
      }
      aLevs$Level[aLevs$Level == tempLabel] <- label
    }
  }
  aLevs
} 


seTaylor <- function(data, res, aggregateBy, formula, wgt, pvs, type) {
  Basic <- unique(data$basic)
  Proficient <- unique(data$proficient)
  Advanced <- unique(data$advanced)
  
  yvar <- all.vars(formula[[2]])
  m <- pvs[1]
  if(type == "discrete") {
    data$Level <- "BBasic"
    data$Level[data[,m] >= Basic & data[,m] < Proficient] <- "Basic"
    data$Level[data[,m] >= Proficient & data[,m] < Advanced] <- "Proficient"
    data$Level[data[,m] >= Advanced] <- "Advanced"
  }
  if(type == "cumulative") {
    data$Level <- "BBasic"
    data$Level[data[,m] >= Basic] <- "Basic"
    data$Level[data[,m] >= Proficient] <- "Proficient"
    data$Level[data[,m] >= Advanced] <- "Advanced"
  }
  
  if(yvar %in% aggregateBy) {
    aggregateBy <- c(aggregateBy, "Level")
  }
  aggregateBy <- aggregateBy[aggregateBy != yvar]
  aggForm <- as.formula(paste("wtdN~", paste(aggregateBy, collapse="+"), sep=" "))
  
  pctAggregationLevel <- length(aggregateBy)
  if(pctAggregationLevel == 0) {
    res$group <- 1
    res$twt <- sum(res$wtdN)
  }
  else {
    twt <- aggregate(aggForm, data = res,  FUN = sum)
    
    names(twt) <- c(aggregateBy,'twt')
    twt$group <- 1:nrow(twt) # used in Taylor series sePct
    res <- merge(res, twt, by = aggregateBy)
    
  }
  res[,"StandardError"] <- NA
  rhs_vars <- unique(c(aggregateBy, "Level", all.vars(formula)))
  rhs_vars <- rhs_vars[rhs_vars != yvar]
  sapply(unique(res$group), function(z) { # for each grouping
    
    resi <- res[res$group == z,]# subset(res, group == z)
    n <- nrow(resi)
    resi$groupsubset <- 1:n
    if(n!=1) { # if n>1 then the percent is not 100 and we need to find StandardError
      pr <- res[res$group==z,"Percent"]/100
      datai <- data
      if(pctAggregationLevel>0) {
        for(i in 1:pctAggregationLevel) {
          datai$rhsi <- datai[,rhs_vars[i]]
          vvv <- as.character(resi[1,rhs_vars[i]])
          datai <- datai[datai$rhsi == vvv,]#subset(datai, rhsi == vvv)
        }
      }
      datai$weight__n__ <- datai[,wgt]
      # identify unit for each obs
      for(i in 1:n) {
        datai$gss <- 1 
        # set gss to 1 for just rows in this group
        for(j in (pctAggregationLevel):length(rhs_vars)) {
          vvv <- as.character(resi[i,rhs_vars[j]])
          datai$gss[datai[,rhs_vars[j]] != vvv] <- 0
        }
        if(sum(datai$gss) >= 1) { # allow for units with no obs that have that
          datai$unit[datai$gss %in% 1] <- i
        }
      }
      # make W where i,jth entry is weight of unit i iff it is in j
      # make \tilde{W} where i,jth entry is weight of unit i
      wtilde <- w <- matrix(0, ncol=n, nrow=nrow(datai))
      # this can be optimized by looping from 1:n instead
      units <- sort(unique(datai$unit))
      for(j in 1:length(units)) { # this could be 1:n but this way it is robust to levels with 0 units in them
        ss <- datai$unit %in% units[j]
        w[ss, j] <- datai[ss, wgt]
      }
      for(j in 1:ncol(wtilde)) {
        #wtilde[,j] <- datai[,wgt]
        wtilde[,j] <- datai[,wgt] * pr[j]
      }
      # again using notation from AM documentation, including multiplicaiton by w
      #uhijw <- w -  t(t(wtilde) * pr) # in uhijw[i,j] j= percent value, i=obs
      uhijw <- w -  wtilde # in uhijw[i,j] j= percent value, i=obs
      colnames(uhijw) <- paste0("v",1:n)
      sumna <- function(x) { sum(x, na.rm=TRUE)}
      meanna <- function(x) { mean(x, na.rm=TRUE)}
      uhijw <- data.frame(uhijw,
                          unit=datai$unit,
                          stratV=datai[,getAttributes(data, "stratumVar")],
                          psuV=datai[,getAttributes(data, "psuVar")])
      for(vi in 1:n) {
        uhijw$v <- uhijw[,paste0("v",vi)]
        uhiw <- aggregate(v ~ stratV + psuV, data=uhijw, FUN=sum)
        uhiw$vv <- ave(uhiw$v, uhiw[,getAttributes(data, "stratumVar")], FUN=meanna)
        uhiw$dx <- uhiw$v - uhiw$vv
        nam <- names(uhiw)
        nam[nam=="v"] <- paste0("uhi",vi)
        nam[nam=="vv"]  <- paste0("uj",vi)
        nam[nam=="dx"]  <- paste0("dx",vi)
        names(uhiw) <- nam
        if(vi == 1) {
          uhiw_ <- uhiw
        } else {
          uhiw_ <- merge(uhiw_, uhiw, by=c(getAttributes(data, "stratumVar"), getAttributes(data, "psuVar")))
        }
      }
      repu <- unique(uhiw_$stratV)
      S <- matrix(0, nrow=nrow(resi), ncol=nrow(resi))
      for(repi in 1:length(repu)) {
        dataii <- uhiw_[uhiw_$stratV == repu[repi],]#subset(uhiw_, stratV == repu[repi])
        jku <- unique(dataii$psuV)
        ni <- length(jku)
        if(ni > 1) {
          for(jkj in 1:ni) {
            #vec <- unlist(subset(dataii, psuV==jku[jkj], paste0("dx",1:n) ,drop=TRUE))
            vec <- unlist(dataii[dataii$psuV==jku[jkj], paste0("dx",1:n),drop=TRUE])
            S <- S + (ni/(ni-1)) * vec %*% t(vec)
          }
        }
      }
      #cb <- sapply(1:n, function(zz) {
      #  rep(resi$WTD_N[zz],n)
      #})
      D <- diag(rep(1/resi$twt[1],nrow(resi)))# * (diag(resi$twt) - cb)
      var <- D %*% S %*% t(D)
      res[res$group==z,"var"] <<- 100* sqrt(diag(var)) # fit to percentage
    } else { # there is only one thing in this aggregation level
      # so the percent will be 100. The frequentist SE on this will be zero.
      res[res$group==z,"var"] <<- 0
    }
  })
  res$twt <- NULL
  res$group <- NULL
  res
}

#' @title Print out a summary of the results of achievementLevels.
#'
#' @description Prints details of discrete and cumulative achievement levels
#'  calculated using weights and variance
#' estimates appropriate for the \code{edsurvey.data.frame}.
#' 
#' @param x             an \code{achievementLevels} object.
#' @param printCall     logical. Set to \code{TRUE} to see details about plausible values and weights used for
#'                      calculating achievement levels.
#' @param printDiscrete logical. Set to \code{TRUE} to print out discrete achievement levels if they are present in \code{x}.
#' @param printCumulative logical. Set to \code{TRUE} to print out cumulative achievement levels if they are present in \code{x}
#' @param ... these arguments are not passed anywhere and are included only for compatibility.
#' @return                A pasted statement vector that shows the details of calculated achievement levels
#' @method print achievementLevels
#' @author Ahmad Emad
#' @export
print.achievementLevels <- function(x, printCall=TRUE, printDiscrete=TRUE, printCumulative=TRUE, ...) {
  if("callVars" %in% names(x)) {
    ll <- "1"
  }
  else {
    ll <- names(x)
  }
  
  for(i in ll) {
    if("callVars" %in% names(x)) {
      x1 <- x
    }
    else {
      x1 <- x[[i]]
    }
    
    if(length(ll) != 1) {
      cat("Output for dataset ", i, "\n")
    }
    
    if(printCall) {
      cv <- x1$callVars
      # comment form PB, I'm not sure what makes the most sense here. Look at print.summary.lm.sdf function
      avs <- paste(cv$achievementVars, collapse=", ")
      p0 <- FALSE
      if(nchar(avs) > 0) {
        cat(paste0("\nAchievementVars: ", avs, "\n"))
        p0 <- TRUE
      }
      ab <- paste(cv$aggregateBy, collapse=", ")
      if(nchar(ab) > 0) {
        if(!p0) {
          cat("\n")
        }
        cat(paste0("aggregateBy: ", ab, "\n"))
        p0 <- TRUE
      }
      if(p0) {
        cat("\n")
      }
      cat(paste0("Achievement Level Cutpoints:\n"))
      cat(cv$cutpoints, "\n\n")
      cat(paste0("Plausible values: ", cv$npv, "\n"))
      cat(paste0("jrrIMax: ", cv$jrrIMax, "\n"))
      cat(paste0("Weight variable: ", sQuote(cv$weight), "\n"))
      cat(paste0("Variance method: ",cv$varMethod,"\n"))
      if(!is.na(cv$njk)) {
        cat(paste0("JK replicates: ", cv$njk, "\n"))
      }
      cat(paste0("full data n: ", x1$n0, "\n"))
      cat(paste0("n used: ", x1$nUsed, "\n"))
      cat("\n")
    }
    if(printDiscrete & !is.null(x1$discrete)) {
      cat("\nDiscrete\n")
      print(x1$discrete, row.names=FALSE) 
    }
    if(printCumulative & !is.null(x1$cumulative)) {
      cat("\nCumulative\n")
      print(x1$cumulative, row.names=FALSE)
    }
    
  }
}

fastAgg <- function(formula, data, FUN) {
  y <- all.vars(formula[[2]])
  x <- all.vars(formula[[3]])
  fun <- substitute(FUN)
  pp <- paste0("as.data.frame(setDT(copy(data))[,.(",y,"=",fun,"(",y,")),by=list(",paste(x,collapse=","),")])")
  eval(parse(text=pp))
}
