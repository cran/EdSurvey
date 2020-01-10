#' @title EdSurvey Percentiles
#'
#' @description Calculates the percentiles of a numeric variable in an
#'              \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'              or an \code{edsurvey.data.frame.list}.
#'
#' @param variable the character name of the variable to percentiles computed,
#'                 typically a subject scale or subscale
#' @param percentiles a numeric vector of percentiles in the range of 0 to 100
#'                    (inclusive)
#' @param data      an \code{edsurvey.data.frame} or an
#'                  \code{edsurvey.data.frame.list}
#' @param weightVar a character indicating the weight variable to use.
#' @param jrrIMax    a numeric value; when using the jackknife variance estimation method, the default estimation option, \code{jrrIMax=1}, uses the 
#'                   sampling variance from the first plausible value as the component for sampling variance estimation. The \eqn{V_{jrr}} 
#'                   term (see 
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}})
#'                   can be estimated with any number of plausible values, and values larger than the number of 
#'                   plausible values on the survey (including \code{Inf}) will result in all plausible values being used. 
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more accurate variance estimates.
#' @param varMethod a character set to \code{jackknife} or \code{Taylor}
#'                  that indicates the variance estimation method used when 
#'                  constructing the confidence intervals. The jackknife
#'                  variance estimation method is always
#'                  used to calculate the standard error.
#' @param alpha a numeric value between 0 and 1 indicating the confidence level.
#'              An \code{alpha} value of 0.05 would indicate a 95\% 
#'              confidence interval and is the default.
#' @param omittedLevels a logical value. When set to the default value of
#'                      \code{TRUE}, drops those levels of 
#'                      all factor variables that are specified in
#'                      \code{achievementVars} and \code{aggregatBy}. 
#'                      Use \code{print} on an \code{edsurvey.data.frame}
#'                      to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value
#'                          of \code{TRUE}, uses the default 
#'                          conditions stored in an \code{edsurvey.data.frame}
#'                          to subset the data. 
#'                          Use \code{print} on an \code{edsurvey.data.frame}
#'                          to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to
#'               \code{NULL}. Can be set as
#'               \code{recode=}\code{list(var1=} \code{list(from=} \code{c("a",}
#'               \code{"b",} \code{"c"),}
#'               \code{to=} \code{"d"))}.
#' @param returnVarEstInputs a logical value set to \code{TRUE} to return the
#'                           inputs to the jackknife and imputation variance
#'                           estimates which allows for the computation
#'                           of covariances between estimates.
#' @param returnNumberOfPSU a logical value set to \code{TRUE} to return the number of 
#'                          primary sampling units (PSUs)                         
#' @param pctMethod one of \dQuote{unbiased} or \dQuote{simple};
#'                  unbiased produces a weighted median unbiased percentile estimate,
#'                  whereas simple uses a basic formula that matches previously
#'                  published results.
#' @param confInt a Boolean indicating if the confidence interval should be returned
#' @details
#' Percentiles, their standard errors, and confidence intervals
#' are calculated according to the vignette titled
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{\emph{Statistical Methods Used in EdSurvey}}.
#' The standard errors and confidence intervals are based
#' on separate formulas and assumptions.
#'
#' The Taylor series variance estimation procedure is not relevant to percentiles
#' because percentiles are not continuously differentiable.
#' 
#' @return
#' The return type depends on whether the class of the \code{data} argument is an
#' \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}.
#'
#' \strong{The data argument is an edsurvey.data.frame}
#'   When the \code{data} argument is an \code{edsurvey.data.frame},
#'   \code{percentile} returns an S3 object of class \code{percentile}.
#'   This is a \code{data.frame} with typical attributes (\code{names},
#'   \code{row.names}, and \code{class}) and additional attributes as follows:
#'     \item{n0}{number of rows on \code{edsurvey.data.frame} before any conditions were applied}
#'     \item{nUsed}{number of observations with valid data and weights larger than zero}
#'     \item{nPSU}{number of PSUs used in the calculation}
#'     \item{call}{the call used to generate these results}
#'
#'   The columns of the \code{data.frame} are as follows:
#'     \item{percentile}{the percentile of this row}
#'     \item{estimate}{the estimated value of the percentile}
#'     \item{se}{the jackknife standard error of the estimated percentile}
#'     \item{df}{degrees of freedom}
#'     \item{confInt.ci_lower}{the lower bound
#'                      of the confidence interval}
#'     \item{confInt.ci_upper}{the upper bound
#'                      of the confidence interval}
#'     \item{nsmall}{the number of units with more extreme results, averaged
#'                   across plausible values}
#'   When the \code{confInt} argument is set to \code{FALSE}, the confidence
#'   intervals are not returned.
#' 
#' \strong{The data argument is an edsurvey.data.frame.list}
#'   When the \code{data} argument is an \code{edsurvey.data.frame.list},
#'   \code{percentile} returns an S3 object of class \code{percentileList}.
#'   This is a data.frame with a \code{call} attribute.
#'   The columns in the \code{data.frame} are identical to those in the previous
#'   section, but there also are columns from the \code{edsurvey.data.frame.list}.
#'   
#'     \item{covs}{a column for each column in the \code{covs} value of the
#'                 \code{edsurvey.data.frame.list}.
#'                 See Examples.}
#'
#' When \code{returnVarEstInputs} is \code{TRUE}, an attribute
#' \code{varEstInputs} also is returned that includes the variance estimate
#' inputs used for calculating covariances with \code{\link{varEstToCov}}.
#'
#' @references
#' Hyndman, R. J., & Fan, Y. (1996). Sample quantiles in statistical packages. \emph{American Statistician}, \emph{50}, 361--365.
#' @author Paul Bailey
#' @importFrom stats reshape
#' @example man/examples/percentile.R
#' @export
percentile <- function(variable, percentiles, data,
                weightVar=NULL, jrrIMax=1,
                varMethod=c("jackknife", "Taylor"),
                alpha=0.05,
                omittedLevels=TRUE,
                defaultConditions=TRUE,
                recode=NULL,
                returnVarEstInputs=FALSE,
                returnNumberOfPSU=FALSE,
                pctMethod=c("unbiased", "simple"),
                confInt=TRUE
                ) {
  # check incoming variables
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  varMethod <- substr(tolower(varMethod[[1]]), 0,1)
  call <- match.call()
  pctMethod <- match.arg(pctMethod)
  
  # check percentiles arguments
  if (any(percentiles < 0 | percentiles > 100)) {
    message(sQuote("percentiles"), " must be between 0 and 100. Values out of range are omitted.")
    percentiles <- percentiles[percentiles >=0 & percentiles <= 100]
  }
  
  if (length(percentiles) == 0) {
    stop("The function requires at least 1 valid percentile. Please check ", sQuote("percentiles"), " argument.")
  }
  if (all(percentiles >= 0 & percentiles <= 1) & any(percentiles != 0)) {
    warning("All values in the ",sQuote("percentiles"), " argument are between 0 and 1. Note that the function uses a 0-100 scale.")
  }
  # deal with the possibility that data is an edsurvey.data.frame.list
  if(inherits(data, "edsurvey.data.frame.list")) {
    ll <- length(data$datalist)
    lp <- length(percentiles)
    # check variable specific to edsurvey.data.frame.list
    call0 <- match.call()
    res <- list(summary=list())
    # because R does partial matching the varialble name for the `data` argument could be a variety of things
    # this code finds its index using the pmatch function
    ln <- length(names(call))
    # this pmatch will return a vector like c(0,0,1,0) if `data` is the third element
    datapos <- which.max(pmatch(names(call), "data", 0L))
    results <- sapply(1:ll, function(i) {
      call[[datapos]] <- data$datalist[[i]]
      tryCatch(eval(call),
               error = function(cond) {
                 message("Error in processing dataset ",sQuote(i), cond)
                 return(data.frame(percentile = percentiles,
                                   estimate = rep(NA,lp),
                                   se = rep(NA,lp),
                                   confInt.ci_lower = rep(NA,lp),
                                   confInt.ci_upper = rep(NA,lp)))
               })
    }, simplify=FALSE)

    # a block consists of the covs and the results for a percentile level:
    resdf <- cbind(data$covs, t(sapply(1:ll, function(ii) { results[[ii]][1,] }, simplify=TRUE)))

    ind <- 2 #iteration starts at second column
    while(ind <= lp) { # lp is the number of percentiles
      # this just grabs blocks, for each percentile level
      newblock <- cbind(data$covs, t(sapply(1:ll, function(ii) {
        results[[ii]][ind,]
      }, simplify=TRUE)))
      # and then appends them to the bottom of the results
      resdf <- rbind(resdf, newblock)
      ind <- ind + 1
    }

    attr(resdf, "call") <- call0
    class(resdf) <- c("percentileList", "data.frame")
    return(resdf)
  } else {
    ####################################### 
    ############### Outline ############### 
    #######################################
    # 1) get data for this variable and weight
    # 2) setup the (x,y,w) data.frame
    # 3) identify requested points
    # 4) Calculate final results

    # clean incoming vars

    # if the weight var is not set, use the default
    if(is.null(weightVar)) {
      wgt <- attributes(getAttributes(data, "weights"))$default
    } else {
      wgt <- weightVar
    } # End of if/else: is.null(weightVar)
    if(min(nchar(wgt)) == 0) {
      # no weight
      stop(paste0("There is no default weight variable for ",getAttributes(data,"survey")," data, so the argument ",sQuote("weightVar"), " must be specified."))
    }

    # 1) get data for this variable and weight
    taylorVars <- NULL
    if(varMethod=="t") {
      taylorVars <- c(getPSUVar(data, weightVar), getStratumVar(data, weightVar))
    }
    getDataVarNames <- c(variable, wgt, taylorVars)
    
    if (returnNumberOfPSU){
      # Get stratum and PSU variable
      stratumVar <- getAttributes(data,"stratumVar")
      psuVar <- getAttributes(data,"psuVar")
      if (all(c(stratumVar, psuVar) %in% names(data)) | all(c(stratumVar, psuVar) %in% colnames(data))) { #names(data$data) changed to colnames(data)::Tom
        getDataVarNames <- unique(c(getDataVarNames, stratumVar, psuVar))
      } else {
        warning("Warning: Stratum and PSU variable are required for this call and are not on the incoming data. Ignoring returnNumberOfPSU=TRUE.")
        returnNumberOfPSU <- FALSE
      }
      
    }
    getDataArgs <- list(data=data,
                        varnames=getDataVarNames,
                        returnJKreplicates=TRUE,
                        drop=FALSE,
                        omittedLevels=omittedLevels,
                        recode=recode,
                        includeNaLabel=TRUE,
                        dropUnusedLevels=TRUE)
    # Default conditions should be included only if the user set it. This adds
    # the argument only if needed
    if(!missing(defaultConditions)) {
      getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
    }
    # edf is the actual data
    edf <- do.call(getData, getDataArgs)
    # check that there is some data to work with
    if(any(edf[,wgt] <= 0)) {
      warning("Removing rows with 0 weight from analysis.")
      edf <- edf[edf[,wgt] > 0,]
    }
    if(nrow(edf) <= 0) {
      stop(paste0(sQuote("data"), " must have more than 0 rows after a call to ",
                  sQuote("getData"), "."))
    }

    # get Plauisble Values of Y variable 
    pvvariable <- hasPlausibleValue(variable, data) # pvy is the plausible values of the y variable
    variables <- variable
    if(pvvariable) {
      variables <- getPlausibleValue(variable, data)
    } else {
      # if not, make sure that this variable is numeric
      edf[,variable] <- as.numeric(edf[,variable])
    }

    jrrIMax <- min(jrrIMax, length(variables))

    # 2) setup the (x,y,w) data.frame
    resdf <- data.frame(inst=1:length(variables))
    for(i in 1:length(percentiles)) {
      resdf[paste0("P",percentiles[i])] <- 0
    }
    
    # get the jackknife replicate weights for this sdf
    jkw <- getWeightJkReplicates(wgt, data)
    # varm: JRR contributions
    # nsmall: minimum (smaller value) of n above or below percentile
    varm <- nsmall <- r <- matrix(NA, nrow=length(variables), ncol=length(percentiles))
    for(vari in 1:length(variables)) {
      # calculate the percentiles for this variable (now vv).
      vv <- variables[vari]
      # make a data frame with x and w on it
      xpw <- data.frame(x=edf[[vv]],
                        w=edf[[wgt]])
      # remove missings
      xpw <- xpw[!is.na(xpw$x),]
      # order the results by x
      xpw <- xpw[ord <- order(xpw$x),]  # keep order as ord for use in sapply later
      # WW is the total weight
      WW <- sum(xpw$w)
      # number of interior points, note extras not added yet
      nn <- nrow(xpw)
      # the percentile of each point,
      # using percentile method recomended by Hyndman and Fan (1996)
      # see percentile vignette for details.
      # xpwc is condensed (duplicate values of x are merged, summing the weight)
      # this is then used for pcpt, while xpw is used for all other pruposes
      xpwc <- pctp(nn, WW, xpw$w, pctMethod, xpw)
      # 3) identify requested points
      # pctf finds the points. the sapply does it for each value in percentiles
      r[vari, ] <- sapply(1:length(percentiles), function(peri) {
                    pctf(percentiles[peri], xpwc, pctMethod) 
                  })
      # get the smaller of n above the cutoff or n below
      # subtract one to account for edge data at the end.
      # max with 0 incase a discrete variable has no cases more extreme
      nsmall[vari, ] <- sapply(1:length(percentiles), function(peri) {
                          # based on xpw, number of observed students
                          max(0, -1 + min( sum(xpw$x < r[vari,peri]), sum(xpw$x > r[vari,peri])  ))
                        })

      # now, estimate the variance of this statistic
      if(vari <= jrrIMax) {
        # get statistic with jackknife weights, this PV
        rprs <- sapply(1:length(percentiles), function(peri) {
                        p <- percentiles[peri]
                        rp <- r[vari, peri]
                        # for all jackknife replicates, find the percentile
                        jkrp <- sapply(1:length(jkw), function(jki) {
                          # xpw has been reordered, so have to reorder
                          # edf in the same way to use it here
                          # set the bottom and top weight to 0 so the sum is still correct 
                          xpw$w <- edf[ord,jkw[jki]]
                          WW <- sum(xpw$w)
                          nn <- nrow(xpw) - 2
                          xpwc <- pctp(nn, WW, xpw$w, pctMethod, xpw)
                          # estimate the percentile with the new weights
                          pctf(p, xpwc, pctMethod)
                        })
                        rpr <- jkrp - rp
                        #if difference rpr is very small round to 0 for DOF correctness 
                        rpr[which(abs(rpr) < (sqrt(.Machine$double.eps)*sqrt(length(jkrp))))] <- 0 
                      
                        rpr
                      })
        
        varm[vari, ] <- apply(rprs^2, 2, sum)
        # get varEstInputs to get dof at end
        varEstInputsJKi <- as.data.frame(rprs)
        varEstInputsJKi$JKreplicate <- 1:nrow(varEstInputsJKi)
        # varEstInputs is supposed to be long, so make it that way.
        varEstInputsJKi <- reshape(varEstInputsJKi, direction="long", varying=1:(ncol(varEstInputsJKi)-1),
                                   v.names="value")
        varEstInputsJKi$PV <- vari
        for(i in 1:length(percentiles)) {
          varEstInputsJKi$variable[varEstInputsJKi$time == i] <- paste0("P", percentiles[i]) 
        }
        varEstInputsJKi$id <- varEstInputsJKi$time <- NULL
        # reorder columns to agree with outer varEstInputs
        varEstInputsJKi <- varEstInputsJKi[,c("PV", "JKreplicate", "variable", "value")]
        if(vari == 1) {
          varEstInputsJK <- varEstInputsJKi
        } else {
          varEstInputsJK <- rbind(varEstInputsJK, varEstInputsJKi)
        }
      } #ends if(vari <= jrrIMax)
    } #ends  for(vari in 1:length(variables))
    M <- length(variables)
    # imputaiton variance / variance due to uncertaintly about PVs
    Vimp <- rep(0, ncol(r))
    if(nrow(r) > 1) {
      # imputation variance with M correction (see stats vignette)
      Vimp <- (M+1)/M * apply(r, 2, var)
    }
    r0 <- apply(r, 2, mean)
    nsmall0 <- apply(nsmall, 2, mean)
    # variance due to sampling
    Vjrr <- getAttributes(data, "jkSumMultiplier") * apply(varm[1:jrrIMax,,drop=FALSE], 2, mean)
    V <- Vimp + Vjrr
    # again, build varEstInputs for dof calculation
    varEstInputsPV <- r
    varEstInputsPV <- as.data.frame(t( t(varEstInputsPV) - apply(varEstInputsPV, 2, mean)))
    varEstInputsPV$PV <- 1:nrow(varEstInputsPV)
    varEstInputsPV <- reshape(varEstInputsPV, direction="long", varying=1:(ncol(varEstInputsPV)-1),
                  v.names="value")
    for(i in 1:length(percentiles)) {
      varEstInputsPV$variable[varEstInputsPV$time == i] <- paste0("P", percentiles[i]) 
    }
    varEstInputsPV$id <- varEstInputsPV$time <- NULL
    # reorder columns to agree with outer varEstInputs
    varEstInputsPV <- varEstInputsPV[,c("PV", "variable", "value")]

    varEstInputs <- list(JK=varEstInputsJK,
                         PV=varEstInputsPV)
    # find confidence interval
    # first, find the variance of the fraction that are above/below this
    # percentile
    if(confInt){
      if(varMethod=="j") {
        #Jackknife method for estimating the variance of the percent below P
        # note: the below works when the are or are not plausible vaues.
        # that is, the section, "Estimation of the standard error of weighted
        # percentages when plausible values are not present, using the jackknife
        # method" is implemented when there are not plausible vlaues present 
        # while the section, "Estimation of the standard error of weighted
        # percentages when plausible values are present, using the jackknife
        # method" is implemented when they are present.

        mu0 <- sapply(1:length(percentiles), function(i) {
          # get the mean of the estimates across pvs
          mean(sapply(1:length(variables), function(vari) {
            # for an individual PV, get the estimated fraction below r0[i]
            vv <- variables[vari]
            xpw <- data.frame(x=edf[[vv]],
                              w=edf[[wgt]])
            xpw <- xpw[!is.na(xpw$x),]
            xpw$below <- (xpw$x <= r0[i])
            s0 <- sum(xpw$w[xpw$below]) / sum(xpw$w) 
          }))
        })

        pVjrr <- pVimp <- matrix(NA, nrow=length(variables), ncol=length(r0))
        # for each PV
        for(vari in 1:length(variables)) {
          # estimate the percentile
          vv <- variables[vari]
          xpw <- data.frame(x=edf[[vv]],
                            w=edf[[wgt]])
          xpw <- xpw[!is.na(xpw$x),]
          # This is one PVs sampling variance, see stats vignette
          if(vari <= jrrIMax) {
            pVjrr[vari,] <- sapply(1:length(r0), function(ri) {
              xpw$below <- (xpw$x < r0[ri])
              # find the proportion of values below this percentile
              s0 <- sum(xpw$w[xpw$below]) / sum(xpw$w) 
              sum(sapply(1:length(jkw), function(jki) {
                xpw$w <- edf[[jkw[jki]]]
                s1 <- sum(xpw$w[xpw$below]) / sum(xpw$w) 
                (s1 - s0)^2
              }))
            })
          }
          if(length(variables) > 1) {
            pVimp[vari,] <- sapply(1:length(r0), function(ri) {
              xpw$below <- (xpw$x < r0[ri])
              # find the values below this percentile
              s0 <- mu0[ri]
              s1 <- sum(xpw$w[xpw$below]) / sum(xpw$w)
              (s1 - s0)^2
            })
          }
        }
        M <- length(variables)
        if(M > 1) {
          # imputation variance estimator, see stats vignette
          pVimp <- (M+1)/(M * (M-1)) * apply(pVimp,2,sum) # will be 0 when there are no PVs
        } else {
          pVimp <- 0
        }
        pVjrr <- apply(pVjrr,2,mean, na.rm=TRUE)
        pV <- pVimp + pVjrr
      } else { # end if(varMethod=="j")
        # Taylor series method for confidence intervals
        # We need the pi value, the proportion under the Pth percentile
        mu0 <- sapply(1:length(percentiles), function(i) {
          # get the mean of the estimates across pvs
          mean(sapply(1:length(variables), function(vari) {
            # for an individual PV, get the estimated fraction below r0[i]
            vv <- variables[vari]
            xpw <- data.frame(x=edf[[vv]],
                              w=edf[[wgt]])
            xpw <- xpw[!is.na(xpw$x),]
            xpw$below <- (xpw$x <= r0[i])
            s0 <- sum(xpw$w[xpw$below]) / sum(xpw$w) 
          }))
        })

        pVjrr <- sapply(1:length(r0), function(ri) {
          # there are two states here, above and below the percentile
          # so the D matrix and Z matrix are 1x1s
          # first calculate D
          # get the sum of weights
          # all xs will have the same missing values, so we can just use the first
          # x because we are just using the missingness
          vv <- variables[1]
          xpw <- data.frame(x=edf[[vv]],
                            w=edf[[wgt]])
          xpw <- xpw[!is.na(xpw$x),]
          sw <- sum(xpw$w)
          D <- 1/sw

          # Now calculate the Z matrix
          sapply(1:length(variables), function(vari) {
            vv <- variables[vari]
            xpw <- data.frame(x=edf[[vv]],
                              w=edf[[wgt]],
                              psuV=edf[,getPSUVar(data, weightVar)],
                              stratV=edf[,getStratumVar(data, weightVar)])
            xpw <- xpw[!is.na(xpw$x),]
            # filter to just stratra that have more than one active PSU
            lengthunique <- function(x) { length(unique(x)) }
            psustrat0 <- aggregate(psuV ~ stratV, data=xpw, FUN=lengthunique)
            # subset to just those units in strata that have more than one active
            # PSU
            names(psustrat0)[names(psustrat0) == "psuV"] <- "psuV_n"
            xpw <- merge(xpw, psustrat0, by="stratV", all.x=TRUE)
            xpw <- xpw[xpw$psuV_n > 1,]
            # now get the mean deviates, S in the statistics vignette
            xpw$below <- (xpw$x <= r0[i])
            xpw$S <- xpw$w * (xpw$below - mu0[ri])

            psustrat <- aggregate(S ~ stratV + psuV, data=xpw, FUN=sum)
            meanna <- function(x) { mean(x, na.rm=TRUE) }
            psustrat$stratum_mu <- ave(psustrat$S, psustrat$stratV, FUN=meanna)
            psustrat$U_sk <- psustrat$S - psustrat$stratum_mu
            psustrat$U_sk2 <- psustrat$U_sk^2
            Z <- sum(psustrat$U_sk2, na.rm=TRUE)
            D*Z*D
          }) #end of sapply(1:length(variables))
        }) #end of sapply pVjrr <- sapply(1:length(r0), function(ri)
        # now calculate Vimp
        pVimp <- matrix(NA, nrow=length(variables), ncol=length(r0))
        if(length(variables) > 1) {
          for(vari in 1:length(variables)) {
            vv <- variables[vari]
            xpw <- data.frame(x=edf[[vv]],
                              w=edf[[wgt]])
            xpw <- xpw[!is.na(xpw$x),]
            # This is one PVs sampling variance
            pVimp[vari,] <- sapply(1:length(r0), function(ri) {
              # find the values below this percentile
              xpw$below <- (xpw$x < r0[ri])
              s0 <- mu0[ri]
              sum(sapply(1:length(jkw), function(jki) {
                xpw$w <- edf[[jkw[jki]]]
                s1 <- sum(xpw$w[xpw$below]) / sum(xpw$w)
                (s1 - s0)^2
              }))
            })
          }
        }
        pVjrr <- apply(pVjrr,2,mean, na.rm=TRUE)
        pVimp <- (M+1)/(M * (M-1)) * apply(pVimp,2,sum) # will be 0 when there are no PVs
        pV <- pVimp + pVjrr
      } # end else for if(varMethod=="j")
      latent_ci_min <- percentiles + 100 * sqrt(pV) * qt(alpha/2,df=62)
      latent_ci_max <- percentiles + 100 * sqrt(pV) * qt(1-alpha/2,df=62)
      latent_ci <- matrix(c(latent_ci_min, latent_ci_max), ncol=2)

      # map back to the variable space
      ci_ <- list()
      for(vari in 1:length(variables)) {
        vv <- variables[vari]
        xpw <- data.frame(x=edf[[vv]],
                          w=edf[[wgt]])
        xpw <- xpw[!is.na(xpw$x),]
        xpw <- xpw[ord <- order(xpw$x),]  # keep order as ord for use in sapply later
        WW <- sum(xpw$w)
        nn <- nrow(xpw) # do not yet account for the first and last row we added
        xpwc <- pctp(nn, WW, xpw$w, pctMethod, xpw)

        ci <- sapply(1:length(percentiles), function(peri) {
                c(pctf(latent_ci[peri,1],xpwc, pctMethod),
                  pctf(latent_ci[peri,2],xpwc, pctMethod))
        })
        ci_ <- c(ci_, list(t(ci)))
      }
      ci <- matrix(NA, nrow=length(r0), ncol=2, dimnames=list(percentiles,c("ci_lower", "ci_upper")))
      for(i in 1:length(r0)) {
        ci[i,1] <- mean(unlist(lapply(ci_, function(mat) { mat[i,1]})))
        ci[i,2] <- mean(unlist(lapply(ci_, function(mat) { mat[i,2]})))
      }
    } # end if(confInt)

    dof <- 0*percentiles
    for(i in 1:length(dof)) {
      dof[i] <- DoFCorrection(varEstA=varEstInputs, varA=paste0("P", percentiles[i]))
    }
    # 4) Calculate final results
    if(confInt) {
      res <- data.frame(percentile=percentiles, estimate=r0, se=sqrt(V),
                        df=dof,
                        confInt=ci,
                        nsmall=nsmall0)
    } else {
      res <- data.frame(percentile=percentiles, estimate=r0, se=sqrt(V),
                        df=dof,
                        nsmall=nsmall0)
    }
    if(returnVarEstInputs) {
      attr(res, "varEstInputs") <- varEstInputs
    }
    attr(res, "n0") <- nrow2.edsurvey.data.frame(data)
    attr(res, "nUsed") <- nrow(edf)
    if (returnNumberOfPSU) {
      attr(res, "nPSU") <- nrow(unique(edf[,c(stratumVar,psuVar)]))
    }
    attr(res, "call") <- call
    class(res) <- c("percentile", "data.frame")
    return(res)
  } # end else for if(inherits(data, "edsurvey.data.frame.list")) {
}

# helper function that gets percentiles from interpolated values
# p the desired percentile
# NOT EXPORTED
pctf <- function(p, xpw, pctMethod) {
  p <- min( max(p,0) , 100) # enforce 0 to 100 range
  # k + 1 (or, in short hand kp1)
  # which.max returns the index of the first value of TRUE
  xpw <- xpw[!duplicated(xpw$p),]
  # kp1 stands for k + 1
  kp1 <- which.max(xpw$p >= p)
  if(kp1==1) {
    return(xpw$x[1])
  }
  if(pctMethod == "simple") {
    return(xpw$x[kp1])
  }
  # k = (k+1) - 1
  k <- kp1 - 1
  pk <- xpw$p[k]
  pkp1 <- xpw$p[kp1]
  gamma <- (p-pk)/(pkp1 - pk)
  #interpolate between k and k+1
  return((1-gamma) * xpw$x[k] + gamma * xpw$x[kp1])
}

# return the percentile of each point
pctp <- function(nn, WW, w, pctMethod, xpw) {
  if(pctMethod == "unbiased") {
    # use average weight within value of x
    # this commented out line does the same as the next two lines
    # xpw$w <- ave(xpw$w, xpw$x, FUN=mean)
    xpwdt <- data.table(xpw)
    xpw <- as.data.frame(xpwdt[, w:=mean(w), by="x"])
    w <- xpw$w
    # see Stats vignette or Hyndman & Fan
    kp <- 1 + (nn-1)/(WW-1) * ((cumsum(c(0,w[-length(w)]))+ (w-1)/2))
    # do not let kp go below 1 nor above nn-1, moves nothing when all weights >= 1
    kp <- pmax(1,pmin(nn-1,kp))
    resp <- 100 * (kp - 1/3) / (nn + 1/3)
  } else {
    resp <- 100 * cumsum(w)/WW
  }
  # this does not cover the entire 0 to 100 interval, so add points on the end.
  xpwc <- rbind(xpw[1,], xpw, xpw[nrow(xpw),])
  xpwc$w[1] <- xpwc$w[nrow(xpwc)] <- 0
  xpwc$p <- c(0,resp,100)
  return(xpwc)
}

#' @method print percentile
#' @export
print.percentile <- function(x, ...) {
  cat("Percentile\nCall: ")
  print(attributes(x)$call)
  cat(paste0("full data n: ", attributes(x)$n0, "\n"))
  cat(paste0("n used: ", attributes(x)$nUsed, "\n"))
  if(!is.null(attributes(x)$nPSU)) {
    cat(paste0("n PSU: ", attributes(x)$nPSU, "\n"))
  }
  cat("\n")
  class(x) <- "data.frame"
  if(min(x$df) <=2) {
    warning("Some degrees of freedom less than or equal to 2, indicating non-finite variance. These estimates should be treated with caution.")
  }
  print(x, row.names=FALSE, ...)
}

#' @method print percentileList
#' @export
print.percentileList <- function(x, ...) {
  cat("percentileList\nCall: ")
  print(attributes(x)$call)
  cat("\n")
  class(x) <- "data.frame"
  print(x, row.names=FALSE, ...)
}
