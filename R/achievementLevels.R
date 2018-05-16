#' @title Achievement Levels
#'
#' @description Returns achievement levels using weights and variance estimates appropriate for the \code{edsurvey.data.frame}.
#'
#' @param achievementVars character vector indicating variables to be included in the achievement 
#'                        levels table, potentially with a subject scale or subscale. When the subject 
#'                        scale or subscale is omitted, then the default subject scale or subscale is 
#'                        used. You can find the default composite scale and all subscales using the 
#'                        function \code{\link{showPlausibleValues}}.
#' @param aggregateBy character vector specifying variables to aggregate achievement levels by. The percentage
#'                    column sums up to 100 for all levels of all variables specified here. When set to 
#'                    default of \code{NULL}, the percentage column sums up to 100 for all 
#'                    levels of all variables specified in \code{achievementVars}.
#' @param data      an \code{edsurvey.data.frame}
#' @param weightVar  character string indicating the weight variable to use.
#'                   Note that only the name of the
#'                   weight variable needs to be included here, and any
#'                   replicate weights will be automatically included.
#'                   When this argument is \code{NULL}, the function uses the default.
#'                   Use \code{\link{showWeights}} to find the default.
#' @param cutpoints numeric vector indicating cutpoints. Set to standard NAEP cutpoints for 
#'                  Basic, Proficient, and Advanced by default.
#' @param jrrIMax  numeric value. When using the jackknife variance estimation method, the \eqn{V_{jrr}} term
#'                 (see Details) can be estimated with any positive number of plausible values and is 
#'                 estimated on the first of the lower of the number of available plausible values and 
#'                 \code{jrrIMax}. When \code{jrrIMax} is set to \code{Inf}, all plausible values will 
#'                 be used. Higher values of \code{jrrIMax} lead to longer computing times and more
#'                 accurate variance estimates.
#' @param omittedLevels  a logical value. When set to the default value (\code{TRUE}), 
#'                       it drops those levels in all factor variables that are specified in \code{achievementVars} 
#'                       and \code{aggregateBy}. Use \code{print} on an \code{edsurvey.data.frame} 
#'                       to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses the default 
#'                          conditions stored in an \code{edsurvey.data.frame} to subset the data. 
#'                          Use \code{print} on an \code{edsurvey.data.frame} to see the default
#'                          conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'               \code{recode = list(var1= list(from=c("a", "b", "c"), to ="d"))}. See Examples.
#' @param returnDiscrete logical indicating if discrete achievement levels should be returned. Defaults 
#'                       to \code{TRUE}.
#' @param returnCumulative logical indicating if cumulative achievement levels should be returned. Defaults
#'                         to \code{FALSE}. The first and last categories are the same as defined for discrete levels.
#' @param returnVarEstInputs a logical value set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates. This is intended to allow for
#'                           the computation
#'                           of covariances between estimates.
#' @author Huade Huo, Ahmad Emad, and Trang Nguyen
#' @details The \code{achievementLevels} function applies appropriate weights and the variance estimation method for each
#'          \code{edsurvey.data.frame}, with several arguments for customizing the aggregation and output of the analysis 
#'          results. Namely, by using these optional arguments, users can choose to generate the percentage of students 
#'          performing at each achievement level (discrete), generate the percentage of students performing at or above each achievement level (cumulative), 
#'          calculate the percentage distribution of students by achievement levels (discrete or cumulative) and 
#'          selected characteristics (specified in \code{aggregateBy}), and compute the percentage distribution of students 
#'          by selected characteristics within a specific achievement level.
#'
#' \subsection{Calculation of percentages}{
#'          The details of the methods are shown in the
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistics vignette} in 
#'          \dQuote{Estimation of Weighted Percentages When Plausible Values Are Present} and are used to calculate 
#'          all cumulative and discrete probabilities.
#'
#'          When the requested achievement levels are discrete (\code{returnDiscrete = TRUE}),
#'          the percentage \eqn{\mathcal{A}} is the percentage of students (within the categories specified in \code{aggregateBy}) 
#'          whose scores lie in the range  \eqn{[cutPoints_i, cutPoints_{i+1}), i = 0,1,...,n}.
#'          \code{cutPoints} is the score thresholds provided by the user with \eqn{cutPoints_0} taken
#'          to be 0. \code{cutPoints} are set to NAEP standard cutpoints for achievement levels by default.
#'          To aggregate by a specific variable, for example, \code{dsex}, specify \code{dsex} in \code{aggregateBy}
#'          and all other variables in \code{achievementVars}. To aggregate by subscale, specify 
#'          the name of the subscale (e.g. \code{num_oper}) in \code{aggregateBy}, and all other variables in 
#'          \code{achievementVars}.
#'          
#'
#'          When the requested achievement levels are cumulative (\code{returnCumulative = TRUE}),
#'          the percentage \eqn{\mathcal{A}} is the percentage of students (within the categories specified in \code{aggregateBy}) 
#'          whose scores lie in the range  [\eqn{cutPoints_i}, \eqn{\infty}), \eqn{i = 1,2...,n-1}. The 
#'          first and last categories are the same as defined for discrete levels.
#' } 
#'         
#' \subsection{Calculation of standard error of percentages}{
#'          The method used to calculate the standard error of the percentages is described in the 
#'          \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistics vignette} 
#'          in the section \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Present, Using the Jackknife Method} 
#'          and \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Not Present, Using the Taylor Series Method.}
#'          For \dQuote{Estimation of the Standard Error of Weighted Percentages When Plausible Values Are Present, Using the Jackknife Method,}
#'         the value of \code{jrrIMax} sets the value of \eqn{m^*}.
#' }
#'          
#' @return
#' A \code{list} containing up to two data frames, one for each of the discrete and cumulative achievement levels
#' as determined by \code{returnDiscrete} and \code{returnCumulative}. The \code{data.frame} contains the following columns:
#' \item{\code{Level}}{one row for each level of the specified achievement cutpoints.}
#' \item{Variables in achievementVars}{one column for each variable in \code{achievementVars} 
#' and one row for each level of each variable in \code{achievementVars}}
#' \item{\code{Percent}}{the percentage of students at or above each achievement level aggregated as specified by \code{aggregateBy}}
#' \item{\code{StandardError}}{the standard error of the percentage, accounting for the survey sampling methodology. 
#'                             See the \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistics vignette}.}
#' \item{\code{n0}}{the number of observations in the incoming data (the
#'                  number of rows when \code{omittedLevels} and
#'                  \code{defaultConditions} are set to \code{FALSE}.}
#' \item{\code{nUsed}}{the number of observations in the data after applying all
#'                     filters (see \code{omittedLevels} and
#'                     \code{defaultConditions}).}
#' @references 
#' Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York, NY: Wiley.
#'
#' @importFrom data.table setDT melt data.table ':=' rbindlist .N .SD as.data.table
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
                              omittedLevels=TRUE,
                              defaultConditions=TRUE,
                              recode = NULL,
                              returnVarEstInputs=FALSE) {
  
  alResultDfList <- list()
  assertArgument(data)
  
  if(inherits(data, "edsurvey.data.frame.list")) {
    ll <- length(data$datalist)
    labels <- as.character(data$covs$labels)
    if(is.null(labels)) {
      labels <- as.character(c(1:ll))
    }
    
    for(i in 1:ll) {
      sdf <- data$datalist[[i]]
      temp <- tryCatch(alResult <- calAL(achievementVars, aggregateBy, sdf, cutpoints, returnDiscrete,
                                         returnCumulative, weightVar, jrrIMax, omittedLevels, defaultConditions,
                                         recode,
                                         defaultConditionsMissing=missing(defaultConditions),
                                         returnVarEstInputs=returnVarEstInputs),
                       error = function(cond) {
                         message(paste("An error occurred while working on data set", labels[i], " excluding results from this data set."))
                         message(cond)
                         return(NULL)
                       }
      )
      if(class(temp) == "achievementLevels") {
        alResultDfList[[labels[i]]] <- alResult
      }
    }
    class(alResultDfList) <- "achievementLevels"
    alResultDfList
  } 
  else {
    alResult <- calAL(achievementVars, aggregateBy, data, cutpoints, returnDiscrete,
                      returnCumulative, weightVar, jrrIMax, omittedLevels, defaultConditions,
                      recode, defaultConditionsMissing=missing(defaultConditions),
                      returnVarEstInputs=returnVarEstInputs)
    alResult
  }
  
}


calAL <- function(achievementVars = NULL, 
                  aggregateBy = NULL,
                  data,
                  cutpoints = NULL,
                  returnDiscrete = TRUE,
                  returnCumulative = FALSE,
                  weightVar=NULL,
                  jrrIMax=1,
                  omittedLevels=TRUE,
                  defaultConditions=TRUE,
                  recode = NULL,
                  defaultConditionsMissing=FALSE,
                  returnVarEstInputs=FALSE) {
  assertArgument(data)
  als <- getAttributes(data, "achievementLevels")
  assertArgument(als)
  
  # Determine if the user supplied variables for calculating achievementlevels, 
  # otherwise just use the default plausible value
  if(is.null(achievementVars)) {
    achievementVars <- attributes(getAttributes(data, "pvvars"))$default
  }
  
  # Check to see only one variable in all the supplied variables has plausible values
  vars <- unique(c(achievementVars, aggregateBy))
  has.more.pvs <- sum(sapply(vars, FUN= function(x) hasPlausibleValue(x, data)))
  assertArgument(has.more.pvs)
  
  # Determine if weight supplied, otherwise use default weight
  wgt <- attributes(getAttributes(data, "weights"))$default 
  
  # Get yvar, if no yvar is specified 
  has.pv <- sum(sapply(vars, FUN= function(x) hasPlausibleValue(x, data)))
  
  if(has.pv == 0) {
    achievementVars <- attributes(getAttributes(data, "pvvars"))$default
    vars <- c(vars, achievementVars)
  }
  
  yvar <- vars[sapply(vars, FUN= function(x) hasPlausibleValue(x, data))]
  assertArgument(yvar)
  achievementVarsNoPV <- vars[sapply(vars, FUN= function(x) !hasPlausibleValue(x, data))]
  aggregateByNoPV <- unlist(sapply(aggregateBy, function(x) {
    if (hasPlausibleValue(x, data)) { return("Level")}
    return(x)
  }))
  
  if (length(aggregateByNoPV) == 0) { aggregateByNoPV <- NULL}  
  
  pvs <- getPlausibleValue(yvar, data)
  jrrIMax <- min(jrrIMax, length(pvs))

  # get the data. This is most of the arguments
  getDataArgs <- list(data=data,
                      varnames=c(vars, wgt),
                      returnJKreplicates=TRUE,
                      drop=FALSE,
                      omittedLevels=omittedLevels,
                      recode=recode,
                      includeNaLabel=TRUE,
                      dropUnusedLevels=TRUE)
  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if(!defaultConditionsMissing) {
    getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
  }
  edfDT <- do.call(getData, getDataArgs)
  edfDT <- setDT(edfDT)
  edfDTnrow <- nrow(edfDT)
  assertArgument(edfDT)
  
  
  if(length(vars[!vars==yvar]) == 0) {
    vars <- c(yvar, "1")
  }
  
  # Determine if the user supplied cutpoints.
  # If the user did not supply cutpoints, get the default ones stored in sdf
  if(is.null(cutpoints)) {
    ALattr <- getAttributes(data, "pvvars")[[yvar]]$achievementLevel
    temp <- as.numeric(ALattr)
    names(temp) <- names(ALattr)
    alsCutpoints <- temp
  } else {
    assertArgument(cutpoints)
    alsCutpoints <- cutpoints
  }
  
  if (is.null(names(alsCutpoints))) {
    names(alsCutpoints) <- paste0("Level ", seq(1, length(alsCutpoints), 1))
  }
  
  jkWeights <- getWeightJkReplicates(wgt, data)
  
  # Gather information about the call.
  # jrrIMax <- min(length(pvs), jrrIMax)
  callVars <- list(achievementVars=achievementVars,
                   aggregateBy=aggregateBy,
                   cutpoints=as.character(sort(unique(alsCutpoints))),
                   weightVar=wgt,
                   jrrIMax=jrrIMax,
                   npv=length(pvs),
                   varMethod="jackknife",
                   njk=length(jkWeights))
  
  # Show levels of achievement Vars
  levelsOfEdfDT <- sapply(edfDT, levels)
  levelsOfEdfDT[sapply(levelsOfEdfDT, is.null)] <- NULL
  levelsOfEdfDTGrid <- expand.grid(levelsOfEdfDT)
  levelsOfEdfDTGrid <- levelsOfEdfDTGrid[,intersect(unique(c(aggregateByNoPV,colnames(levelsOfEdfDTGrid))), colnames(levelsOfEdfDTGrid)), drop = FALSE] 
  # Calculate discrete achievement levels
  discreteDT <- recodeEdf(edfDT, pvs, alsCutpoints, returnCumulative = FALSE)
  discreteOrder <- sortALResults(alsCutpoints, returnCumulative = FALSE)
  discreteResult <- calculateAL(recodeEdfResults = discreteDT, pvs = pvs, 
                                jrrIMax = jrrIMax, returnVarEstInputs = returnVarEstInputs,
                                achievementVars = achievementVarsNoPV, aggregateBy=aggregateByNoPV,
                                wgt = wgt, jkSumMultiplier=getAttributes(data, "jkSumMultiplier"),
                                jkWeights = jkWeights)
  discreteDT <- NULL
  # Report rows with missing percent/N/wtdN
  if (length(achievementVarsNoPV) != 0) {
    discreteLevelAndAchievementVars <- merge(discreteOrder,levelsOfEdfDTGrid)
  } else {
    discreteLevelAndAchievementVars <- as.data.frame(discreteOrder)
  }
  names(discreteLevelAndAchievementVars)[1] <- "Level"
  discreteLevelAndAchievementVars$Level <- factor(discreteLevelAndAchievementVars$Level, levels = discreteOrder)
  # Order the columns
  order_cols <- intersect(colnames(discreteLevelAndAchievementVars),vars)
  order_cols <- unique(c(aggregateByNoPV, "Level", order_cols))
  if (length(order_cols) > 0) {
    for (i in length(order_cols):1) {
      discreteLevelAndAchievementVars <- discreteLevelAndAchievementVars[order(discreteLevelAndAchievementVars[,order_cols[i]]),,drop=FALSE]
    }
  }
 
 
  discreteResultWithNA <- merge(discreteLevelAndAchievementVars, discreteResult[[1]], all.x = TRUE, sort = FALSE)
  discreteResultWithNA[c("N", "wtdN", "Percent")][is.na(discreteResultWithNA[c("N", "wtdN", "Percent")])] <- 0
  sortedDiscreteResult <- discreteResultWithNA
  rownames(sortedDiscreteResult) <- NULL
  
  
  # Calculate cummulative achievement levels
  if (returnCumulative){
    cumulativeOrder <- sortALResults(alsCutpoints, returnCumulative = TRUE)
    cumulativeResults <- discreteResult[[1]][discreteResult[[1]]$Level == head(cumulativeOrder, 1), ]
    # Return JK and PV result if returnVarEstInputs is TRUE
    if (returnVarEstInputs) {
      JKcumulativeResults <- discreteResult[[2]][discreteResult[[2]]$variable == head(cumulativeOrder, 1), ]
      PVcumulativeResults <- discreteResult[[3]][discreteResult[[3]]$variable == head(cumulativeOrder, 1), ]
    }
    # Calculate result for individual cumulative level
    for (i in 1:(length(alsCutpoints) - 1)) {
      cumulativeDTLevelI <- recodeEdf(edfDT, pvs, alsCutpoints, returnCumulative = TRUE, 
                                      cumulativeLevel = i)
      cumulativeDTLevelIResult <- calculateAL(recodeEdfResults = cumulativeDTLevelI, pvs = pvs, 
                                              jrrIMax = jrrIMax, returnVarEstInputs = returnVarEstInputs,
                                              achievementVars = achievementVarsNoPV, aggregateBy = aggregateByNoPV,
                                              wgt = wgt, jkSumMultiplier=getAttributes(data, "jkSumMultiplier"), jkWeights = jkWeights)
      cumulativeResults <- rbind(cumulativeResults, cumulativeDTLevelIResult[[1]])
      # Return combined JK and PV result if returnVarEstInputs is TRUE
      if (returnVarEstInputs) {
        JKcumulativeResults <- rbind(JKcumulativeResults, cumulativeDTLevelIResult[[2]])
        PVcumulativeResults <- rbind(PVcumulativeResults, cumulativeDTLevelIResult[[3]])
      }
      
    }
    edfDT <- NULL
    cumulativeResults <- rbind(cumulativeResults, 
                               discreteResult[[1]][discreteResult[[1]]$Level == tail(cumulativeOrder, 1), ])
    
    # Report rows with missing percent/N/wtdN
    if (length(achievementVarsNoPV) != 0) {
      cumulativeLevelAndAchievementVars <- merge(cumulativeOrder,levelsOfEdfDTGrid)
    } else {
      cumulativeLevelAndAchievementVars <- as.data.frame(cumulativeOrder)
    }
    names(cumulativeLevelAndAchievementVars)[1] <- "Level"
    
    # Order cumulative level results
    cumulativeLevelAndAchievementVars$Level <- factor(cumulativeLevelAndAchievementVars$Level, levels = cumulativeOrder)
    order_cols <- intersect(colnames(cumulativeLevelAndAchievementVars),vars)
    order_cols <- unique(c(aggregateByNoPV,"Level", order_cols))
    if (length(order_cols) > 0) {
      for (i in length(order_cols):1) {
        cumulativeLevelAndAchievementVars <- cumulativeLevelAndAchievementVars[order(cumulativeLevelAndAchievementVars[,order_cols[i]]),,drop=FALSE]
      }
    }
   
    cumulativeResultsWithNA <- merge(cumulativeLevelAndAchievementVars, cumulativeResults, all.x = TRUE, sort = FALSE)
    cumulativeResultsWithNA[c("N", "wtdN", "Percent")][is.na(cumulativeResultsWithNA[c("N", "wtdN", "Percent")])] <- 0
    sortedCumulativeResults <- cumulativeResultsWithNA
    
    rownames(sortedCumulativeResults) <- NULL
    # Order returnVarEstInputs results
    if (returnVarEstInputs) {
      JKcumulativeResults <- rbind(JKcumulativeResults, discreteResult[[2]][discreteResult[[2]]$variable == tail(cumulativeOrder, 1), ])
      sortedJKcumulativeResults <- JKcumulativeResults[order(JKcumulativeResults$PV, 
                                                             JKcumulativeResults$JKreplicate), ]
      rownames(sortedJKcumulativeResults) <- NULL
      PVcumulativeResults <- rbind(PVcumulativeResults, discreteResult[[3]][discreteResult[[3]]$variable == tail(cumulativeOrder, 1), ])
      sortedPVcumulativeResults <- PVcumulativeResults[order(PVcumulativeResults$PV), ]
      rownames(sortedPVcumulativeResults) <- NULL
    }
  }
  
  # Create a list containing the achievement levels and the call information
  res <- list(callVars=callVars)
  if(returnDiscrete) {
    res <- c(res, list(discrete=sortedDiscreteResult))
  }
  if(returnCumulative) {
    res <- c(res, list(cumulative=sortedCumulativeResults))
  }
  # Gather and order returnVarEstInputs results
  if(returnVarEstInputs) {
    if(returnDiscrete) {
      sortedJKDiscreteResult <- discreteResult[[2]]
      rownames(sortedJKDiscreteResult) <- NULL
      sortedPVDiscreteResult <- discreteResult[[3]]
      rownames(sortedPVDiscreteResult) <- NULL
      
      #assign to 0 if values are close to 0 
      sortedJKDiscreteResult$value[which(abs(sortedJKDiscreteResult$value) < (sqrt(.Machine$double.eps)*sqrt(max(sortedJKDiscreteResult$JKreplicate))))] <- 0 
      
      res <- c(res, list(discVarEstInputs=list(JK=sortedJKDiscreteResult, 
                                               PV=sortedPVDiscreteResult)))
    }
    if(returnCumulative) {
      #assign to 0 if values are close to 0 for correct DOF correction
      sortedJKcumulativeResults$value[which(abs(sortedJKcumulativeResults$value) < (sqrt(.Machine$double.eps)*sqrt(max(sortedJKcumulativeResults$JKreplicate))))] <- 0 
      
      res <- c(res, list(cumVarEstInputs=list(JK = sortedJKcumulativeResults,
                                              PV = sortedPVcumulativeResults)))
    }
  }
  res <- c(res, list(n0=nrow2.edsurvey.data.frame(data),nUsed=edfDTnrow))
  class(res) <- "achievementLevels"
  res
  
}

assertArgument <- function(arguments){
  argumentName <- deparse(substitute(arguments))
  switch(argumentName,
         "data"= {
           # Check data supplied are correct
           checkDataClass(arguments, c("edsurvey.data.frame", 
                                       "light.edsurvey.data.frame", 
                                       "edsurvey.data.frame.list"))
         },
         "als" = {
           cutpoints <- get("cutpoints", envir = parent.frame())
           if(arguments[1] == "Not Found" & is.null(cutpoints)) {
             stop(paste0("Default achievement levels not found. The ", sQuote("cutpoints"), " argument must be set."))
           }
         },
         "has.more.pvs" = {
           if(arguments > 1) {
             stop(paste0("More than one variable with associated plausible values found in the ",
                         sQuote("achievementVars"), " or ", sQuote("aggregateBy"), " arguments."))
           }
         },
         "yvar" = {
           # Check to see if at least one plausible value variable identified
           if(length(arguments) == 0) {
             stop(paste0("At least one variable in ", sQuote("achievementVars"), " must have plausible values to calculate achievement levels."))
           }
         },
         "edfDT" = {
           # check if there is any data
           if(nrow(arguments) <= 0) {
             stop(paste0(sQuote("data"), " must have more than 0 rows after a call to ", sQuote("getData"), "."))
           }
         },
         "cutpoints" = {
           # check if cutpoints are numeric
           if (!is.numeric(arguments)) {
             stop("cutpoints must be numeric values.")
           }
         }
  )
}

sortALResults <- function(als, returnCumulative) {
  sortList <- paste0("Below ", names(als)[1])
  if (!returnCumulative){
    for (i in 1:length(als)) {
      sortList <- c(sortList, paste0("At ", names(als)[i]))
    }
  } else {
    for (i in 1:(length(als) - 1)) {
      sortList <- c(sortList, paste0("At or Above ", names(als)[i]))
    }
    sortList <- c(sortList, paste0("At ", tail(names(als), n = 1)))
  }
  return(sortList)
}

# Recode edf with achievement levels
recodeEdf <- function(edfDT, pvs, als, returnCumulative, cumulativeLevel = 0){
  levelCols <- paste0(pvs,"_lvl")
  # clean existing level columns
  junk <- intersect(levelCols, names(edfDT))
  if (length(junk) > 0) {
    edfDT <- edfDT[,(junk) := NULL]
  }
  
  # build level columns for each pv
  if (!returnCumulative) {
    discreteLabels <- c(paste0("Below ", names(als)[1]), paste0("At ", names(als)[1]))
    if (length(als) > 1) {
      discreteLabels <- c(discreteLabels, paste0("At ", names(als)[2:length(als)]))
    }
    edfDT <- edfDT[,(levelCols) := lapply(.SD, function(c) {
      cut(c, breaks = c(-Inf, als, Inf), labels = discreteLabels, right = FALSE)
    }),
    .SDcols = pvs]
  } else {
    edfDT <- edfDT[,(levelCols) := lapply(.SD, function(c) {
      ifelse(c < als[cumulativeLevel],"Other", paste0("At or Above ", names(als)[cumulativeLevel]))
    }),
    .SDcols = pvs]
  }
}


# Estimation of weighted percentages when plausible values are present
calculateAL <- function(recodeEdfResults, pvs, jrrIMax, returnVarEstInputs, achievementVars, aggregateBy, wgt, jkSumMultiplier, jkWeights){
  # result list
  # 1. res: discrete or cumulative result by Level and achievementVars/ aggregateBy variables
  # 2. VarEstImput: JK (jrr) by PV (jrrIMAx), Level, achievementVars, jk
  # 3. VarEstInput: PV (imp) by PV (M/ npv), Level, achievementVars
  alList <- list()
  
  # Preparation: Construct 2 important data.tables for later calculation
  M <- length(pvs)
  suppressWarnings(W <- sum(recodeEdfResults[,get(wgt)])) # total population weight
  # imp_dt is used for res and VarEstInput$PV
  
  suppressWarnings(imp_dt <- lapply(1:M,function(i) {
    recodeEdfResults[,list(lengthY = .N, sumY = sum(get(wgt))), by = c(paste0(pvs[i],"_lvl"),achievementVars)][,PV:=i]
  }))
  imp_dt <- rbindlist(imp_dt)
  names(imp_dt)[1] <- "Level"
  # end imp_dt

  #jrr_dt is used for res (SE) and VarEstInput$JK
  jrr_dt <- lapply(1:jrrIMax, function(i) {
    recodeEdfResults[,lapply(.SD,sum), by = c(paste0(pvs[i],"_lvl"),achievementVars), .SDcols = jkWeights][,PV:=i]
  })
  jrr_dt <- rbindlist(jrr_dt)
  names(jrr_dt)[1] <- "Level"
  # end jrr_dt
  
  # 1. Calculate weighted percentages (N, wtdN, Percent)
  res <- imp_dt[,list(wtdN=sum(sumY), N = sum(lengthY)), by = c("Level",achievementVars)]
  res <- res[, Percent:=wtdN * 100 / sum(.SD$wtdN), by = aggregateBy]
  res <- res[,`:=`(wtdN = wtdN/M, N = N/M)]

  # add 'value' to imp_dt
  imp_dt <- merge(imp_dt, res[,c("Level", achievementVars, "Percent"), with = FALSE], 
    by = c("Level", achievementVars), all.x = TRUE, all.y = TRUE)
  
  imp_dt <- imp_dt[,pcti:=100*sumY/sum(.SD$sumY), by = c("PV",aggregateBy)][,value:=(Percent - pcti)]

  # add 'value' to jrr_dt
  jrr_dt <- merge(jrr_dt, imp_dt[,c("PV","Level",achievementVars,"pcti"), with = FALSE], by = c("PV","Level",achievementVars),
    all.x = TRUE, all.y = FALSE)
  jrr_dt <- jrr_dt[,(jkWeights):=lapply(.SD, function(jj) 100*jj/sum(jj) - pcti), .SDcols = jkWeights, by = c("PV",aggregateBy)][,pcti:=NULL]
  jrr_dt <- melt(jrr_dt, id.vars = c("PV","Level", achievementVars),
                 variable.name = "JKreplicate", value.name = "value")

  # 2. Estimate standard error of weighted percentages when plausible values
  # are present, using the jackknife method (SE)
  # a. res$Vjrr
  res <- merge(res, jrr_dt[,list(Vjrr = jkSumMultiplier * sum(value^2)/jrrIMax), 
                           by = c("Level",achievementVars)], sort = FALSE, by = c("Level", achievementVars), all.x = TRUE)
  # b. res$Vimp
  res <- merge(res, imp_dt[,list(Vimp = sum(value^2)*(M+1)/(M*(M-1))), by = c("Level", achievementVars)], 
               sort = FALSE, by = c("Level", achievementVars), all.x = TRUE)
  
  # c. StandardError
  res <- res[,StandardError:= sqrt(Vjrr + Vimp)]
  alList[[1]] <- as.data.frame(res)[,c("Level",achievementVars, "N", "wtdN","Percent","StandardError")]
  
  # 3. Return VarEstInputs (JK and PV)
  if (returnVarEstInputs) {
    # JK
    jrr_dt <- jrr_dt[Level!="Other"][,JKreplicate := as.integer(gsub("[^0-9]","",JKreplicate))]
    jrrResultOrder <- c("PV","JKreplicate", achievementVars,"Level","value")
    jrr_dt <- as.data.frame(jrr_dt)[,jrrResultOrder]
    names(jrr_dt) <- c("PV","JKreplicate", achievementVars,"variable","value")
    # rearrange output
    for (i in c("variable","JKreplicate","PV")) {
      jrr_dt <- jrr_dt[order(jrr_dt[,i]),]
    }
    jrr_dt$variable <- as.character(jrr_dt$variable)
    alList[[2]] <- jrr_dt
    # PV
    imp_dt <- imp_dt[Level!="Other"][,PV := as.integer(gsub("[^0-9]","",PV))]
    impResultOrder <- c("PV", achievementVars, "Level", "value")
    imp_dt <- as.data.frame(imp_dt)[,impResultOrder]
    names(imp_dt) <- c("PV", achievementVars, "variable", "value")
    # rearrange output
    for (i in c("variable","PV")){
      imp_dt <- imp_dt[order(imp_dt[,i]),]
    }
    imp_dt$variable <- as.character(imp_dt$variable)
    alList[[3]] <- imp_dt
  }
  return(alList)
}

#' @title Print AchievementLevels Results
#'
#' @description Prints details of discrete and cumulative achievement levels
#'  calculated using weights and variance
#' estimates appropriate for the \code{edsurvey.data.frame}.
#' 
#' @param x             an \code{achievementLevels} object
#' @param printCall     a logical value; by default (\code{TRUE}), prints details about plausible 
#'                      values and weights used for calculating achievement levels.
#' @param printDiscrete a logical value; by default (\code{TRUE}), prints discrete achievement 
#'                      levels if they are present in \code{x}.
#' @param printCumulative a logical value; by default (\code{TRUE}), prints cumulative achievement 
#'                        levels if they are present in \code{x}
#' @param ... these arguments are not passed anywhere and are included only for compatibility.
#' @method print achievementLevels
#' @author Huade Huo and Ahmad Emad 
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
