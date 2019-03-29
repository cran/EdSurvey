#' @title EdSurvey Mixed-Effects Model
#' 
#' @description Fits a linear or logistic weighted mixed-effects model.
#' 
#' @param formula a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}}
#'                for the multilevel regression or mixed model. See Examples and the vignette titled 
#'                \emph{\href{https://www.air.org/sites/default/files/EdSurvey-Mixed_Models.pdf}{Methods Used for Estimating Mixed-Effects Models in EdSurvey}}
#'                for more details on how to specify a mixed model. If \emph{y} is
#'                left blank, the default subject scale or subscale variable
#'                will be used. (You can find the default using
#'                \code{\link{showPlausibleValues}}.) If \emph{y} is a variable
#'                for a subject scale or subscale (one of the names shown by
#'                \code{\link{showPlausibleValues}}), then that subject scale
#'                or subscale is used.
#'                
#'                For logistic models, we recommend using the \code{I()} function 
#'                   to define the level used for success. (See Examples.)
#' @param data an \code{edsurvey.data.frame} or a \code{light.edsurvey.data.frame}
#' @param weightVars character vector indicating weight variables for
#'                   corresponding levels to use. The \code{weightVar} must be
#'                   the weights for the \code{edsurvey.data.frame}. The weight variables
#'                   must be in the order of level (from lowest to highest level).
#' @param weightTransformation a logical value to indicate whether the function
#'                             should standardize weights before using it in the
#'                             multilevel model. If set to \code{TRUE}, the
#'                             function will look up standard weight
#'                             transformation methods often used for a specific
#'                             survey. Weight transformation can be found in the vignette titled 
#'                             \emph{\href{https://www.air.org/sites/default/files/EdSurvey-Mixed_Models.pdf}{Methods Used for Estimating Mixed-Effects Models in EdSurvey}}.
#'                             If set to \code{FALSE} or if the survey of the specified \code{data} does
#'                             not have a standard weight transformation method,
#'                             raw weights will be used.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                  \code{recode=}\code{list(}\code{var1} \code{=} \code{list(}\code{from=} \code{c("a",} \code{"b",} \code{"c"),} \code{to=} \code{"d"))}. 
#'                  See Examples in \ifelse{latex}{\code{lm.sdf}}{\code{\link{lm.sdf}}}.
#' @param defaultConditions a logical value. When set to the default value of
#'                          \code{TRUE}, uses the default conditions stored in
#'                          an \code{edsurvey.data.frame} to subset the data.
#'                          Use \code{print} on an \code{edsurvey.data.frame}
#'                          to see the default conditions.
#' @param tolerance a numeric value to indicate how accurate the result is.
#'                  See Details for more information. Defaults to 0.01.
#' @param nQuad an integer to indicate the number of quadrature points in
#'              adaptive quadrature process. See documentation of
#'              \ifelse{latex}{\code{WeMix::mix}}{\code{\link[WeMix]{mix}}} 
#'              for more details on how \code{nQuad} affects
#'              the estimation. If no \code{nQuad} is set, function will start with 
#'              \code{nQuad = 5} and increase until a stable result is reached. See details for 
#'              further discussion. 
#' @param verbose an integer; when set to \code{1}, it will print out
#'                the brief progress of the function \code{mix.sdf}.
#'                Users can use these traced messages for further diagnosis.
#'                When set to \code{2}, it will print
#'                out the detailed progress, including temporary estimates
#'                   during the optimization. Defaults to \code{0}, which 
#'                   will run the function without output. 
#' @param family an element of \ifelse{latex}{\code{family}}{\code{\link[stats]{family}}} class; optionally used 
#'                to specify generalized linear mixed models.
#'                Defaults to \code{NULL}, which runs mixed linear regression models. Another 
#'                family option is \code{binomial(link="logit")} to run binomial mixed models.
#' @param centerGroup a list in which the name of each element is the name of the aggregation level, 
#' and the element is a formula of variable names to be group mean centered. For example, to group mean center
#'  gender and age within the group student: \code{list("student"= ~gender+age)}. Defaults to \code{NULL}, which means 
#'  predictors are not adjusted by group centering. See Examples in \ifelse{latex}{\code{mix}}{\code{\link[WeMix]{mix}}}.
#' @param centerGrand  a formula of variable names to be grand mean centered. For example, to center the 
#' variable education by overall mean of education: \code{~education}. Defaults to \code{NULL}, which means predictors
#' are not adjusted by grand centering.
#' @param fast a logical value; when set to \code{TRUE}, use  c++ function for faster result. Defaults to \code{FALSE}.
#'  
#' @param ... other potential arguments to be used in \ifelse{latex}{\code{mix}}{\code{\link[WeMix]{mix}}} 
#' @return
#' A \code{mixedSdfResults} object with the following elements:
#'    \item{call}{the original call used in \code{mixed.sdf} }
#'    \item{formula}{the formula used to fit the model}
#'    \item{coef}{the estimates of the coefficients}
#'    \item{se}{the standard error estimates of the coefficients}
#'    \item{vars}{variance components of the model }
#'    \item{levels}{the  number of levels in the model }
#'    \item{ICC}{the intraclass correlation coefficient of the model }
#'    \item{npv}{the number of plausible values}
#'    \item{ngroups}{a \code{data.frame} that includes number of observations for each group}
#' If the formula does not involve plausible values, the function will return the following additional elements:
#'    \item{lnlf}{the likelihood function } 
#'    \item{lnl}{the log-likelihood of the model }
#' If the formula involves plausible values, the function will return the following additional elements:
#'    \item{Vimp}{the estimated variance from uncertainty in the scores}
#'    \item{Vjrr}{the estimated variance from sampling}
#'    
#' @details If users do not specify the \code{nQuad} argument, the functions will
#'          use the \code{tolerance} argument to repeatedly run the mixed model and increment
#'          \code{nQuad} (starting at 5) until the percentage of difference between the
#'          log-likelihood of the new model and the old model is smaller than
#'          \code{tolerance}. If users provide  \code{nQuad}, selecting a smaller value of \code{nQuad} 
#'          can save processing time; however, it is recommended that users try incrementing
#'            \code{nQuad} to check whether the result is stable.
#'          
#'          Note that if the outcome variable has plausible values, the previous setting will be 
#'          applied to the estimation of all plausible values. 
#' 
#' @references 
#' Rabe-Hesketh, S., & Skrondal, A. (2006). Multilevel modelling of complex
#' survey data. \emph{Journal of the Royal Statistical Society: Series A
#' (Statistics in Society), 169}(4), 805--827.
#' 
#' 
#' @author Trang Nguyen and Claire Kelley
#' @seealso \ifelse{latex}{\code{mix}}{\code{\link[WeMix]{mix}}} and \ifelse{latex}{\code{lm.sdf}}{\code{\link{lm.sdf}}}
#' @example \man\examples\mixed.sdf.R
#' @export
#' @importFrom WeMix mix
#' @importFrom utils getFromNamespace
#' @importFrom lme4 lFormula
mixed.sdf <- function(formula, 
                      data, 
                      weightVars=NULL,
                      weightTransformation=TRUE,
                      recode=NULL,
                      defaultConditions=TRUE, 
                      tolerance=0.01,
                      nQuad=NULL,
                      verbose=0,
                      family=NULL,
                      centerGroup=NULL,
                      centerGrand=NULL,
                      fast=FALSE,
                      ...) {
  call <- match.call()
  if(!missing(nQuad) & is.null(family)) {
    warning(paste0("The ", sQuote("nQuad"), " argument is deprecated for linear models."))
  }
  if(!missing(tolerance) & is.null(family)) {
    warning(paste0("The ", sQuote("tolerance"), " argument is deprecated."))
  }
  if(!missing(fast)) {
    warning(paste0("The ", sQuote("fast"), " argument is deprecated."))
  }
  # if users specify an edsurvey.data.frame or light.edsurvey.data.frame,
  # weightVars will be defined for each supported survey if not provided.
  # for each survey supported by edsurvey
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))
  # if the weight var is not set, use the default
  survey <- getAttributes(data, "survey")
  if (is.null(weightVars)) {
    if (survey == "PISA") {
      weightVars <- c("w_fstuwt","w_fschwt")
    } else if (survey %in% c("TIMSS", "TIMSS Advanced")) {
      weightVars <- c("totwgt","schwgt")
    } else {
      stop("mixed.sdf currently only supports automated weights for PISA, TIMSS, and TIMSS Advanced. If you use another survey, please specify your own weights. ")
    }
    call$weightVars <- weightVars # update weightVars argument in the call
  }
  
  if (!inherits(formula, "formula")){
    stop(paste0(sQuote("formula"), " argument must be of class formula."))
  }
  
  # check if there is an outcome variable and set it to the default if it is missing
  zeroLengthLHS <- attr(terms(formula), "response") == 0
  if(zeroLengthLHS) {
    yvar <- attributes(getAttributes(data, "pvvars"))$default
    formula <- update(formula, new=substitute( yvar ~ ., list(yvar=as.name(yvar))))
  } else{
    yvar <- all.vars(formula[[2]])
  } # End of if/else: if (zeroLengthLHS)
  #check if LHS is pv and if so get all values and set flag "pv" to TRUE
  pv <- hasPlausibleValue(yvar,data)
  yvars <- yvar
  if(pv){
    yvars <- getPlausibleValue(yvar,data)
  }
  

  # Get data
  getDataArgs <- list(data=data,
                      varnames=unique(c(all.vars(formula), weightVars, yvars)),
                      returnJKreplicates=TRUE,
                      drop=FALSE,
                      omittedLevels=FALSE,
                      recode=recode,
                      includeNaLabel=TRUE,
                      dropUnusedLevels=TRUE)
  
  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if(!missing(defaultConditions)) {
    getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
  }
  
  # edf is the actual data
  edf <- do.call(getData, getDataArgs)
  rawN <- nrow(edf)
  
  # drop zero-weight case
  for(wgt in weightVars) {
    if(any(!(!is.na(edf[,wgt]) & edf[,wgt] > 0))) {
      warning("Removing ",sum(!(!is.na(edf[,wgt]) & edf[,wgt] > 0))," rows with 0 or NA weight on ",dQuote(wgt)," from analysis.")
      edf <- edf[!is.na(edf[,wgt]) & edf[,wgt] > 0,]
    }
  }
  
  # deal with plausible values 
  pvy <- hasPlausibleValue(yvar, data) # pvy is the plausible values of the y variable
  yvars <- yvar
  lyv <- length(yvars)
  if(any(pvy)) {
    yvars <- getPlausibleValue(yvar, data)
  } else {
    # if not, make sure that this variable is numeric
    edf[,"yvar"] <- as.numeric(eval(formula[[2]],edf))
    formula <- update(formula, new=substitute( yvar ~ ., list(yvar=as.name(yvar))))
    yvars <- "yvar"
  } # End of if statment: any(pvy)
  
  yvar0 <- yvars[1]
  # this allows that variable to not be dynamic variable, it is explicitly defined to be yvar0
  if(!is.null(family) && family$family %in% c("binomial")) { #if using binomial set y to 1 only if it has the highest value
    if(any(pvy)) {
      for(i in 1:length(yvars)) {
        # PV, so we have not evaluated the I() yet (if any)
        for(yvi in 1:length(pvy)) {
          if(pvy[yvi]) {
            edf[,yvar[yvi]] <- edf[,getPlausibleValue(yvar[yvi], data)[i]]
          }
        }
        edf[,yvars[i]] <- as.numeric(eval(formula[[2]],edf))
      }
      oneDef <- max(edf[,yvars], na.rm=TRUE)
      for(i in yvars) {
        edf[,i] <- ifelse(edf[,i] %in% oneDef, 1, 0)
      }
    } else {
      # for non-PV, I() has been evaluated
      oneDef <- max(edf[,yvars], na.rm=TRUE)
      edf[,yvar0] <- ifelse(edf$yvar %in% oneDef, 1, 0)
    }
  }
  
  # Get Group names
  formula <- update(formula, as.formula(paste0(yvar0," ~ .")))
  
  # start by getting the lme4 parsed formula
  lformula <- lFormula(formula=formula, data=edf)
  
  # get the group names (ie level 2+ variables) from the formula
  # get the unparsed group names, this could include, e.g. a:b
  unparsedGroupNames <- names(lformula$reTrms$cnms)
  # a function to parse these
  groupParser <- function(groupi) {
    # have base::all.vars parse the group name
    all.vars(formula(paste0("~",groupi)))
  }
  
  # apply the parser to each random effect, and unique them
  groupNames <- rev(unique(unlist(lapply(unparsedGroupNames, groupParser))))
  
  # checking whether level matches requirements
  if (length(groupNames) == 0) {
    stop("The formula only indicates one level. Use lm.sdf instead.")
  }
  
  if (length(weightVars) != length(groupNames) + 1) {
    stop(paste0("The model requires ", length(groupNames) + 1, " weights."))
  }
  
  level <- length(groupNames) + 1
  
  # Construct standardized weights called 'pwt1' for level 1 weights and 'pwt2' for level 2 weights
  # The standardization method varies across different surveys
  if (!weightTransformation) {
    # no weight transformations
    edf$pwt1 <- edf[,weightVars[1]]
    edf$pwt2 <- edf[,weightVars[2]]
  } else {
    # weight transformations by survey
    if (survey == "PISA") {
      edf$sqw <- edf[,weightVars[1]]^2
      sumsqw <- aggregate(as.formula(paste0("sqw ~ ", groupNames)), data = edf, sum)
      sumw <- aggregate(as.formula(paste0(weightVars[1],"~", groupNames)), data = edf, sum)
      edf$sumsqw <- sapply(edf[,groupNames], function(s) sumsqw$sqw[sumsqw[,groupNames] == s])
      edf$sumw <- sapply(edf[,groupNames], function(s) sumw[sumw[,groupNames] == s, weightVars[1]])
      edf$pwt1 <- edf[,weightVars[1]] * (edf$sumw / edf$sumsqw)
      edf$pwt2 <- edf[,weightVars[2]]
      edf$sqw <- NULL
      edf$sumsqw <- NULL
      edf$sumw <- NULL
    } else if (survey %in% c("TIMSS", "TIMSS Advanced")) {
      edf$pwt1 <- edf[,weightVars[1]] / edf[,weightVars[2]]
      edf$pwt2 <- edf[,weightVars[2]]
    } else { # For other survey data in edsurvey, users need to specify their own weights
      warning(paste0("EdSurvey currently does not specify weight transformation rules for ",survey,". Raw weights were used for the analysis."))
      edf$pwt1 <- edf[,weightVars[1]]
      edf$pwt2 <- edf[,weightVars[2]]
    }
  } # end if (!weightTransformation)
  
  # Remove omittedLevels from the data.frame edf
  lev <- unlist(getAttributes(data, "omittedLevels")) # here we already know it is an edsurvey.data.frame or light.edsurvey.data.fram
  keep <- rep(0, nrow(edf))
  for (i in 1:ncol(edf)) {
    vari <- names(edf)[i]
    keep <- keep + (tolower(edf[,vari]) %in%  tolower(lev))
  }
  if(sum(keep>0) > 0) {
    # only omit if something gets omitted
    edf <- edf[keep==0,,drop=FALSE]
  }
  
  #make copy of formula to be modified during interations through plausible values. 
  formula_pv <- formula
  
  #load summary function from WeMix
  summary.WeMixResults <- getFromNamespace("summary.WeMixResults","WeMix")
  
  if(!pv){
    # there are no plausible values, just a single outcome
    res <- run_mix(nQuad=nQuad, call=call, formula=formula, edf=edf,
                   verbose=verbose, family=family,center_group=centerGroup,center_grand=centerGrand,
                   tolerance=tolerance, fast=fast, ...)
    
    #get enrivonment of WeMix likelihood function to later extract covariance from
    env <- environment(res$lnlf)
    
    #use summary to extract variances. 
    model_sum <- summary.WeMixResults(res)
    res$se <- c(model_sum$coef[,2] , model_sum$vars[,2])
    res$vars <- model_sum$vars[,1]
    
    # Remove some return values
    res$CMODE <- NULL 
    res$CMEAN <- NULL
    res$hessian <- NULL
    
  } else { #run the plausible values version 
    #iterate through the plausible values 
    results <- list()
    variances <- list()
    pvi <- 0
    for (value in yvars){
      if (verbose) {
        cat("Estimation for ",value, " started.\n")
      }
      pvi <- pvi+1
      
      #for each iteration update formula to have the plausible value we are dealing with as the dependent
      formula_pv <- update(formula_pv,as.formula(paste(value,"~.")))
      
      model <- withCallingHandlers(run_mix(nQuad=nQuad, call=call, formula=formula_pv,
                                           edf=edf, verbose=verbose,family=family,center_group=centerGroup,center_grand=centerGrand,
                                           tolerance=tolerance, fast=fast, ...),
                                   warning = function(w) {
                                     if (pvi != 1) { # only print the warning the first time
                                       invokeRestart("muffleWarning")
                                     } else {
                                       message(conditionMessage(w))
                                     }
                                   },
                                   message = function(c) {
                                     if (pvi != 1) {
                                       invokeRestart("muffleMessage")
                                     }
                                   })
      results[[value]] <- model 
      #use summary to extract variances
      model_sum <- summary.WeMixResults(model)
      variances[[value]] <- c(model_sum$coef[,2]^2 , model_sum$vars[,2]^2)
    }
    res <- results[[1]]
    
    #NULL out things that dont exist for PVs
    res$lnl <- NULL
    res$lnlf <- NULL
    res$CMODE <- NULL 
    res$CMEAN <- NULL
    res$hessian <- NULL
    #get enrivonment of WeMix likelihood function to later extract covariance from
    env <- environment(results[[1]]$lnlf)
    
    #Coefficients are just average value
    avg_coef <- rowSums(matrix(sapply(results,function(x){x$coef}),nrow=length(results[[1]]$coef)))/length(yvars)
    names(avg_coef) <- names(results[[1]]$coef)
      
     #calcuate imputation variance for SEs and Residuals 
    imputation_var <- ((length(yvars)+1)/((length(yvars)-1)*length(yvars))) * rowSums(matrix(sapply(results, function(x){x$coef - avg_coef})^2,nrow=length(avg_coef))) 
    
    res$coef <- avg_coef
    #Variation in coefficients comes from imputation and also sampling 
    sampling_var <- colSums(Reduce(rbind,variances))/length(yvars)
    names(sampling_var) <- c(names(avg_coef), names(results[[1]]$vars))
    
    #total se is sqrt of sampling + imputation variance
    res$se <- sqrt(sampling_var[1:length(imputation_var)] + imputation_var)
    
    #ICC is mean ICC
    res$ICC <-  tryCatch(sum(sapply(results,function(x){x$ICC}))/length(yvars),
                         error=function(cond) {
                           return(NA)
                         })
    
    #average residual variance
    res$vars <-  rowSums(matrix(sapply(results,function(x){x$vars}), nrow = length(results[[1]]$vars)))/length(yvars)
    names(res$vars) <- names(results[[1]]$vars)
    imputation_var_for_vars <- ((length(yvars)+1)/((length(yvars)-1)*length(yvars))) * rowSums(matrix(sapply(results, function(x){x$vars - res$vars})^2,nrow=length(res$vars))) 
    names(imputation_var_for_vars) <- names(res$vars)
    
    #Add on SE of residuals from sandwich estimator
    res$se <-  c(res$se,sqrt(sampling_var[(length(imputation_var)+1):length(sampling_var)] + imputation_var_for_vars))
    
    res$Vimp <- c(imputation_var, imputation_var_for_vars)
    res$Vjrr <- sampling_var
  } #end if(pv)

  
  res$npv <- length(yvars)
  res$n0 <- rawN
  res$nUsed <- nrow(edf)
  
  #get group numbers 
  covCon <- get("cConstructor", env)
  lmeVarDf <- get("covMat", environment(covCon))
  lmeVarDf <- lmeVarDf[,c("grp","ngrp","level")]
  names(lmeVarDf) <- c("Group Var","Observations","Level")
  res$ngroups <- lmeVarDf
  
  class(res) <- "mixedSdfResults"
  return(res)
}



# helper function
run_mix <- function(nQuad,call,formula,edf,verbose,tolerance,family,center_group,center_grand, fast, ...){
  
  verboseAll <- ifelse(verbose==2,TRUE,FALSE) #set verbosity for WeMix to true if overall verbosity is 2
  if (!is.null(nQuad)) {
    if(verbose > 0) {
      message(sQuote("nQuad"), " argument is specified so ", sQuote("tolerance"), " argument will not be used. It's recommended that users try incrementing ", sQuote("nQuad"), " to check whether the estimates are stable. ")
    }
    res <- mix(formula, data=edf, weights=c("pwt1","pwt2"), verbose = verboseAll, nQuad = nQuad,family=family,center_group=center_group,center_grand=center_grand, fast=fast, ...)
    call$tolerance <- NULL
    res$call <- call
    return(res)
  } else {
    # specify a starting nQuad
    nQuad <- 5
    diff <- Inf
    if (verbose>0) {
      cat("Trying nQuad = ",nQuad,".\n") 
    }
    res0 <- mix(formula, data=edf, weights=c("pwt1","pwt2"), verbose = verboseAll, nQuad = nQuad,family=family,
                center_group=center_group,center_grand=center_grand, fast=fast, ...)

    #estimate0 <- c(coef(res0), res0$vars, res0$lnl)
    while(diff > tolerance) {
      nQuad <- nQuad + 2
      if (verbose>0) {
        cat("Trying nQuad = ",nQuad,".\n") 
      }
      res <- mix(formula, data=edf, weights=c("pwt1","pwt2"), verbose = verboseAll, nQuad = nQuad,family=family,center_group=center_group,center_grand=center_grand, fast=fast, ...)
       # diff is the percentage difference of new lnl and old lnl
      diff <- abs(res$lnl - res0$lnl)/abs(res0$lnl)
      res0 <- res
    }
    
    # Return results
    call$nQuad <- nQuad
    res$call <- call
    class(res) <- "mixedSdfResults"
    return(res)
  } #end else if(!is.null(nQuad))
  
}


#' @method summary mixedSdfResults
#' @importFrom stats printCoefmat
#' @export
summary.mixedSdfResults <- function(object, ...) {
  #in the plausible values case there is no lnl and variance is already calcuated 
  object$coef <- cbind(Estimate=object$coef, "Std. Error"=object$se[1:length(object$coef)], "t value"=object$coef/object$se[1:length(object$coef)])
  object$vars <- cbind(variance=object$vars, "Std. Error"=object$se[-(1:nrow(object$coef))],"Std.Dev."=sqrt(object$vars))
  
  class(object) <- "summary.mixedSdfResults"
  object
}


#' @method print summary.mixedSdfResults
#' @export
print.summary.mixedSdfResults <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat(paste0("\nFormula: ", paste(deparse(x$call$formula), collapse=""),"\n\n"))

  if(x$npv>1){
    cat("\nPlausible Values: ",x$npv)
  }
  cat("\nNumber of Groups: \n")
  print(x$ngroups)
  
  cat("\nVariance terms:\n")
  print(x$vars)
  cat("\nFixed Effects:\n")
  printCoefmat(x$coef)
  if(x$npv==1){ #only print lnl if non plausible values case
    cat("\nlnl=",format(x$lnl,nsmall=2),"\n")
  }
  if (!is.na(x$ICC)) {
    cat("\nIntraclass Correlation=",format(x$ICC, nsmall=3, digits=3),"\n")
  }
}

