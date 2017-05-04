#' @title Run a linear model on an edsurvey.data.frame.
#'
#' @description Fits a linear model that uses weights and variance estimates appropriate for the \code{edsurvey.data.frame}.
#'
#' @param formula    a \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}} for the
#'                   linear model. See \ifelse{latex}{\code{lm}}{\code{\link[stats]{lm}}}.
#'                   If \emph{y} is left blank, the default subject scale or subscale variable
#'                   will be used. (You can find the default using
#'                   \code{\link{showPlausibleValues}}.)
#'                   If \emph{y} is a variable for a subject scale or subscale (one of the
#'                   names shown by \code{\link{showPlausibleValues}}),
#'                   then that subject scale or subscale is used.
#' @param data       an \code{edsurvey.data.frame}.
#' @param weightVar  character indicating the weight variable to use (see Details).
#'                   The \code{weightVar} must be one of the weights for the
#'                   \code{edsurvey.data.frame}. If \code{NULL}, uses the default
#'                   for the \code{edsurvey.data.frame}.
#' @param varMethod  A character set to \dQuote{jackknife} or \dQuote{Taylor} that indicates the variance
#'                   estimation method to be used. (See Details.)
#' @param jrrIMax    when using the jackknife variance estimation method, the \eqn{V_{jrr}} term
#'                   (see Details) can be estimated with
#'                   any positive number of plausible values and is estimated on the first of
#'                   the lower
#'                   of the number of available plausible values and \code{jrrIMax}. When
#'                   \code{jrrIMax} is set to \code{Inf}, all of the plausible values will be used.
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more
#'                   accurate variance estimates.
#' @param relevels   a list. Used when the user wants to change the contrasts from the
#'                   default treatment contrasts to treatment contrasts with a chosen omitted
#'                   group.
#'                   To do this, the user puts an element on the list named the same name as
#'                   a variable
#'                   to change contrasts on
#'                   and then makes the value for that list element equal to the value
#'                   that should
#'                   be the omitted group. (See Examples.)
#' @param schoolMergeVarStudent a character variable name from the student file used to
#'                              merge student and school data files. Set to \code{NULL} by default.
#' @param schoolMergeVarSchool  a character variable name name from the school file used
#'                              to merge student and school data files. Set to \code{NULL}
#'                              by default.
#' @param omittedLevels a logical value. When set to the default value of \code{TRUE}, drops
#'                      those levels of all factor variables that are specified
#'                      in \code{edsurvey.data.frame}. Use \code{print} on an
#'                      \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses
#'                          the default conditions stored in \code{edsurvey.data.frame}
#'                          to subset the data. Use \code{print} on an
#'                          \code{edsurvey.data.frame} to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                  recode = list(var1= list(from=c("a,"b","c"), to ="d")). (See examples.)
#'
#' @details 
#'  
#' This function implements an estimator that correctly handles left hand
#' side variables that are either numeric or plausible values, allows for survey 
#' sampling weights and estimates variances using the jackknife replication method.
#' The Statistics vignette describes estimation of the reported statistics. 
#' (Run \code{vignette("statistics", package="EdSurvey")} at the R prompt to see the vignette.)
#' 
#' Regardless of the variance estimation, the \bold{coefficients} are estimated
#' using the sample weights according to the section titled
#' \dQuote{estimation of weighted means when plausible values are not present.}
#' or the section titled
#' \dQuote{estimation of weighted means when plausible values are present.}
#' depending on if there are assessment variables or variables with plausible values
#' in them.
#' 
#' How the standard errors of the coefficients are estimated depends on the
#' value of \code{varMethod} and the presence of plausible values (assessment variables),
#' But, once it is obtained the \emph{t} statistic
#' is given by \deqn{t=\frac{\hat{\beta}}{\sqrt{\mathrm{var}(\hat{\beta})}}} where
#' \eqn{ \hat{\beta} } is the estimated coefficient and \eqn{\mathrm{var}(\hat{\beta})} is
#' its variance of that estimate. The \emph{p}-value associated with the coefficient
#' is then calculated using the number of jackknife replicates as the degrees of freedom.
#' 
#' The \bold{coefficient of determination (R-squared value)} is similarly estimated by finding
#' the average R-squared using the sample weights for each set of plausible values.
#'
#'
#' \subsection{Variance estimation of coefficients}{
#'   All variance estimation methods are shown in the \dQuote{Statistics} vignette.
#'
#'   When \code{varMethod} is set to \dQuote{jackknife} and the predicted
#'   value does not have plausible values, the variance of the coefficients
#'   is estimated according to the section,
#' \dQuote{Estimation of standard errors of weighted means when
#'         plausible values are not present, using the jackknife method.}
#'
#'   When plausible values are present and \code{varMethod} is \dQuote{jackknife,} the
#'   the variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of standard errors of weighted means when
#'         plausible values are present, using the jackknife method.}
#'
#'   When plausible values are not present and \code{varMethod} is \dQuote{Taylor,} the
#'   the variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of standard errors of weighted means when plausible
#'         values are not present, using the Taylor series method.}
#'
#'   When plausible values are present and \code{varMethod} is \dQuote{Taylor,} the
#'   the variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of standard errors of weighted means when plausible
#'         values are present, using the Taylor series method.}
#' }
#'
#' @references
#' Binder, D. A. (1983). On the Variances of Asymptotically Normal Estimators From Complex Surveys. \emph{International Statistical Review}, 51(3): 279--92. 
#'
#' Rubin, D. B. (1987). \emph{Multiple Imputation for Nonresponse in Surveys}. New York, NY: Wiley.
#'
#' Weisberg, S. (1985). \emph{Applied Linear Regression} (2nd ed.). New York, NY: Wiley.
#'
#' @return
#' An \code{edsurvey.lm} with elements:
#'    \item{call}{The function call.}
#'    \item{formula}{The formula used to fit the model.}
#'    \item{coef}{The estimates of the coefficients.}
#'    \item{se}{The standard error estimates of the coefficients.}
#'    \item{Vimp}{The estimated variance due to uncertainty in the scores (plausible values variables).}
#'    \item{Vjrr}{The estimated variance due to sampling.}
#'    \item{M}{The number of plausible values.}
#'    \item{varm}{The variance estimates under the various plausible values.}
#'    \item{coefm}{The values of the coefficients under the various plausible values.}
#'    \item{coefmat}{The coefficient matrix (typically produced by the summary of a model).}
#'    \item{r.squared}{The coefficient of determination.}
#'    \item{weight}{The name of the weight variable.}
#'    \item{npv}{Number of plausible values.}
#'    \item{njk}{The number of jackknife replicates used. Set to NA when Taylor series variance estimtes are used.}
#'    \item{varMethod}{One of \dQuote{Taylor series} or \dQuote{jackknife.}}
#'
#' @seealso \ifelse{latex}{\code{lm}}{\code{\link[stats]{lm}}}
#' @author Paul Bailey
#'
#' @example man/examples/lm.sdf.R
#' @importFrom stats lm
#' @importFrom stats aggregate
#' @importFrom stats pt
#' @importFrom stats relevel
#' @importFrom stats model.matrix
#' @importFrom stats lm.wfit
#' @importFrom stats as.formula
#' @export
lm.sdf <- function(formula,
                   data,
                   weightVar=NULL,
                   relevels=list(),
                   varMethod=c("jackknife", "Taylor"),
                   jrrIMax=1,
                   schoolMergeVarStudent=NULL,
                   schoolMergeVarSchool=NULL,
                   omittedLevels=TRUE,
                   defaultConditions=TRUE,
                   recode=NULL) {
  # outline:
  # 1) check, format inputs
  # 2) get the data
  # 3) deal with relevels.
  # 4) deal with yvar having plausible values
  # 5) run the main regression
  # 6) run the regressions, form the inputs for variance estimation
  # 7) form output, including final variance estimation

  # 1) check, format inputs
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  call <- match.call()
  sdf <- data # short for survey data.frame
  # check that incoming arguments are generally acceptable
  if(!inherits(formula, "formula")) {
    stop(paste0("The argument ", sQuote("formula"), " must be a formula."))
  }
  
  if(nrow(sdf) <= 0) {
    stop(paste0(sQuote("data"), " must have more than 0 rows."))
  }
  # get just the first element of varMethod. Note that this works for NULL
  varMethod <- varMethod[[1]] 
  if(!inherits(varMethod,"character") || nchar(varMethod) < 1 ||!tolower(varMethod) %in% substr(c("jackknife", "taylor"),0,nchar(varMethod))) {
    stop(paste0("The argument ",sQuote("varMethod"),
                " must be one of ", dQuote("jackknife"),
                " or ", dQuote("Taylor"), "."))
  }
  varMethod <- substr(tolower(varMethod[[1]]), 0,1)
  
  # if the weight var is not set, use the default
  if(is.null(weightVar)) {
    wgt <- attributes(getAttributes(sdf, "weights"))$default
  } else {
    wgt <- weightVar
  } # End of if/else: is.null(weightVar)
  
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
  if(varMethod=="t") {
    taylorVars <- c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar"))
    jrrIMax <- NA
  }
  # 2) get the data
  # This is most of the arguments
  getDataArgs <- list(data=sdf,
                   varnames=c(all.vars(formula), wgt, taylorVars),
                   returnJKreplicates=(varMethod=="j"),
                   drop= FALSE,
                   schoolMergeVarStudent=schoolMergeVarStudent,
                   schoolMergeVarSchool=schoolMergeVarSchool,
                   omittedLevels=omittedLevels,
                   recode = recode,
                   includeNaLabel=TRUE,
                   dropUnusedLevels=TRUE)

  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if(!missing(defaultConditions)) {
    getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
  }
  # edf is the actual data
  edf <- do.call(getData, getDataArgs)
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
                    " argument, can't find the variable named ",sQuote(vari),"."))
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
                    paste(sQuote(lvls), collapse=", "), "."))
      } # End of if statment !relevels[[i]] %in% lvls
      edf[,vari] <- relevel(edf[,vari], ref=relevels[[i]])
    } # end for(i in 1:length(relevels))
  } # end if(length(relevels) > 0) 
  
  # 4) deal with yvar having plausible values
  pvy <- hasPlausibleValue(yvar, sdf) # pvy is the plausible values of the y variable
  yvars <- yvar
  if(pvy) {
    yvars <- getPlausibleValue(yvar, sdf)
  } else {
    # if not, make sure that this variable is numeric
    edf[,yvars] <- as.numeric(edf[,yvars])
  } # End of if statment: pvy

  # 5) run the main regression

  # run a regression, starting with the first PV or maybe the only outcome variable
  yvar0 <- yvars[1]
  # this allows that variable to not be dynamic variable, it is explicitly defined to be yvar0
  edf$yvar0 <- edf[,yvar0]
  frm <- update(formula, yvar0 ~ .)
  edf$w <- edf[,wgt]
  lm0 <- lm(frm, data=edf, weights=w)
  # this grabs the list of weight variable names
  wgtl <- getAttributes(sdf, "weights")[[wgt]]
  varEstInputs <- list()
  # there is now a very long set of conditionals
  # 6) run the regressions, form the inputs for variance estimation
  if(pvy) { # the y variable is a plausible value
    # this if condition takes care of the Taylor series method as well as the jackknife method
    # for equations, see the statistics vignette
    jrrIMax <- min(jrrIMax, length(yvars))
    if(varMethod=="t") {
      jrrIMax <- length(yvars)
    } 
    # varm is the variance matrix by coefficient and PV (for V_jrr)

    varm <- matrix(NA, nrow=jrrIMax, ncol=length(coef(lm0)))
    # coefficients by PV
    coefm <- matrix(NA, nrow=length(yvars), ncol=length(coef(lm0)))
    # R-squared by PV
    r2s <- vector(length=length(yvars))
    if(varMethod == "j") {
      # y is a variable with plausible values and we are using the JK1 approach
      for(pvi in 1:jrrIMax) {
        coefa <- matrix(NA, nrow=length(wgtl$jksuffixes), ncol=length(coef(lm0)))
        edf$yvar0 <- edf[,yvars[pvi]]
        edf$w <- edf[,wgt]
        lmi <- lm(frm, data=edf, weights=w)
        co0 <- coef(lmi)
        r2s[pvi] <- summary(lmi)$r.squared
        coefm[pvi,] <- co0
        X_lmi <- model.matrix(frm, edf)
        Y_lmi <- edf[,as.character(frm[[2]])]
        for(jki in 1:length(wgtl$jksuffixes)) {
          w_lmi <- edf[,paste0(wgtl$jkbase, wgtl$jksuffixes[jki])]
          lmi <- lm.wfit(x = X_lmi, y = Y_lmi, w = w_lmi)
          coefa[jki,] <- coef(lmi)
        } # End of for loop: jki in 1:length(wgtl$jksuffixes)
         
        
        coefa <- t( (t(coefa) - co0))
        coefa <- coefa^2
        varm[pvi,] <- getAttributes(data, "jkSumMultiplier") * apply(coefa, 2, sum)
      } # End of for loop: (pvi in 1:jrrIMax)
      while(pvi < length(yvars)) {
        pvi <- pvi + 1
        edf$yvar0 <- edf[,yvars[pvi]]
        edf$w <- edf[,wgt]
        co0 <- coef(lmi <- lm(frm, data=edf, weights=w))
        r2s[pvi] <- summary(lmi)$r.squared
        coefm[pvi,] <- co0
      } # End while loop: pvi < length(yvars)
    } else { # End of if statment: varMethod == "j"  # Taylor series
      # y is a variable with plausible values and we are using the Taylor series approach
      vari <- names(relevels)[1]
      # here the regression uses the normal equations for weighted regression
      # the QR decomposition is used to improve stability but
      # the D matrix involves an inverse that cannot be avoided
      X <- sparse.model.matrix(frm, edf)
      XW <- X * sqrt(edf[,wgt,drop=TRUE])
      qrXW <- qr(XW)
      W <- Diagonal(n=nrow(edf),edf[,wgt,drop=TRUE])
      D <- solve(t(X) %*% W %*% X)
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
        # from here on out this section is pretty dense
        # notation (uhij) from AM documentation
        # this is the partial of the likelihood at the unit level
        edf$uhij <- e*as.matrix(X)
        for(bi in 1:length(b)) { # for each coefficient
          coln <- colnames(edf$uhij)[bi]
          # get the stratum/PSU based sum
          edf$bb <- edf$uhij[,bi] * edf[,wgt]
          taylorVars <- c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar"))
          resi <- aggregate(formula(paste0("bb ~ ", getAttributes(data, "psuVar"), " + ", getAttributes(data, "stratumVar"))), edf, sum)
          # and the average of the same across strata
          resj <- aggregate(formula(paste0("bb ~ ", getAttributes(data, "stratumVar"))),resi,function(x) { mean(x)})
          names(resi)[3] <- coln
          names(resj)[2] <- paste0("uh_",coln)
          if(bi==1) {
            uhi <- resi
          } else{
            uhi <- merge(uhi,resi, by=c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar")), all=TRUE)
          }
          uhi <- merge(uhi, resj, by=c(getAttributes(data, "stratumVar")), all=TRUE)
          uhi[,paste0("dx",bi)] <- uhi[,coln] - uhi[,paste0("uh_",coln)]
        }
        # this will be the variance-covariance matrix for this plausible value
        vv <- matrix(0,nrow=length(b), ncol=length(b))
        sa <-  lapply(unique(uhi[,getAttributes(data, "stratumVar")]), function(ii) {
          vvj <- matrix(0,nrow=length(b), ncol=length(b))
          unkj <- unique(uhi[uhi[,getAttributes(data, "stratumVar")] == ii, getAttributes(data, "psuVar"), drop=TRUE])
          if(length(unkj)>1) { # cannot estimate variance off one unit
            sb <- lapply(unkj, function(jj) {
              v <- as.numeric(t(uhi[uhi[,getAttributes(data, "stratumVar")]==ii & uhi[,getAttributes(data, "psuVar")]==jj, paste0("dx",1:length(b)), drop=FALSE]))
              vvj <<- vvj + v %*% t(v)
            })
            vvj <- vvj * ( (length(unkj)) / ( length(unkj) - 1) )
            vv <<- vv + vvj
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
        varm[mm,] <<- as.numeric(diag(vc))
      })
    } # End of if/else statment: varMethod == "j"
    # number of PVs
    M <- length(yvars)
    # imputaiton variance / variance due to uncertaintly about PVs
    Vimp <- (M+1)/M * apply(coefm, 2, var)
    r2 <- mean(r2s)
    coef <- apply(coefm, 2, mean)
    # variance due to sampling
    Vjrr <- apply(varm[1:jrrIMax,,drop=FALSE], 2, mean)
    V <- Vimp + Vjrr
  } else { # end if(pvy)
    # the y variable is not a plausible value
    # this section handles jackknife and Taylor series estimation
    if(varMethod == "j") {
      # jackknife variance estimation
      coefa <- matrix(NA, nrow=length(wgtl$jksuffixes), ncol=length(coef(lm0)))
      co0 <- coef(lm0)
      r2 <- summary(lm0)$r.squared
      
      for(jki in 1:length(wgtl$jksuffixes)) {
        edf$w <- edf[,paste0(wgtl$jkbase, wgtl$jksuffixes[jki])]
        lmi <- lm(frm, data=edf, weights=w)
        coefa[jki,] <- coef(lmi)
      }

      coefa <- t( (t(coefa) - co0)) # conservative JK estimator
      njk <- length(wgtl$jksuffixes)
      coefa <- coefa^2 # this is JK-2
      Vjrr <- getAttributes(data, "jkSumMultiplier") * apply(coefa, 2, sum)
      coef <- co0 # coefficients come from full sample weights run
      Vimp <- 0 # no imputation variance when there are no PVs
      M <- 1 # only on replicate when there are no PVs
      varm <- NULL # no variance by PV
      coefm <- NULL # no coefficients matrix by PV
    } else { # end if(varMethod == "j")
      # Taylor series
      vari <- names(relevels)[1]
      X <- sparse.model.matrix(frm, edf)
      W <- Diagonal(n=nrow(edf),edf[,wgt,drop=TRUE])
      XW <-X * sqrt(edf[,wgt,drop=TRUE])
      qrXW <- qr(XW)
      D <- solve(t(X) %*% W %*% X)
      Y <- as(edf[,yvars[1],drop=FALSE],"Matrix")
      YW <- Y * sqrt(edf[,wgt,drop=TRUE])
      # next line gets b in a less stable way than the line after it based on QR
      #b <- D %*% t(X) %*% W %*% Y
      b <- qr.coef(qrXW, YW)
      # next line works, but is it more stable? Instead use the obvious formula.
      #e <- (1/sqrt(edf[,"origwt",drop=TRUE]))* as.vector(qr.resid(qrXW, YW))
      fitted <- X%*%b
      e <- as.vector((Y-fitted))
      # s2 is not needed
      #s2 <- sum( e^2)/(length(e)-length(b) )
      # notation from AM documentation
      # this is the partial of the likelihood at the unit level
      edf$uhij <- e*as.matrix(X)
      
      for(bi in 1:length(b)) { # for each coefficient
        coln <- colnames(edf$uhij)[bi]
        # get the stratum/PSU based sum
        edf$bb <- edf$uhij[,bi] * edf[,wgt] #edf$origwt
        resi <- aggregate(formula(paste0("bb ~ ", getAttributes(data, "psuVar"), " + ", getAttributes(data, "stratumVar"))), edf, sum)
        # and the average of the same across strata
        resj <- aggregate(formula(paste0("bb ~ ", getAttributes(data, "stratumVar"))),resi,function(x) { mean(x)})
        names(resi)[3] <- coln
        names(resj)[2] <- paste0("uh_",coln)
        if(bi==1) {
          uhi <- resi
        } else{
          uhi <- merge(uhi,resi, by=c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar")), all=TRUE)
        }
        uhi <- merge(uhi, resj, by=c(getAttributes(data, "stratumVar")), all=TRUE)
        uhi[,paste0("dx",bi)] <- uhi[,coln] - uhi[,paste0("uh_",coln)]
      } # End of for loop: bi in 1:length(b)
      # this will be the variance-covariance matrix for this plausible value
      vv <- matrix(0,nrow=length(b), ncol=length(b))
      sa <-  lapply(unique(uhi[,getAttributes(data, "stratumVar")]), function(ii) {
        vvj <- matrix(0,nrow=length(b), ncol=length(b))
        #unkj <- unique(subset(uhi, repgrp1 == ii, getAttributes(data, "psuVar"), drop=TRUE))
        unkj <- unique(uhi[uhi[,getAttributes(data, "stratumVar")] == ii, getAttributes(data, "psuVar"), drop=TRUE])
        if(length(unkj)>1) { # cannot estimate variance off one unit
          sb <- lapply(unkj, function(jj) {
            v <- as.numeric(t(uhi[uhi[,getAttributes(data, "stratumVar")]==ii & uhi[,getAttributes(data, "psuVar")]==jj, paste0("dx",1:length(b)), drop=FALSE]))
            vvj <<- vvj + v %*% t(v)
          })
          vvj <- vvj * ( (length(unkj)) / ( length(unkj) - 1) )
          vv <<- vv + vvj
        } # End of if statment:  if(length(unkj)>1)
      }) # End of Lappy Loop: lapply(unique(uhi$repgrp1), function(ii)
      M <- vv
      vc <- D %*% M %*% D
      # get R-squared
      meanY <- sum(W %*% Y) / sum(diag(W))
      dY <- Y-meanY
      syy <- t(dY) %*% W %*% dY # Weisberg and adding in weights
      rss <- t(e) %*% W %*% e # Weisberg, page 81 eq 4.2
      r2 <- as.numeric(1-(rss/syy)) # Weisberg, page 49 eq 2.31
      coef <- as.numeric(b)
      Vjrr <- as.numeric(diag(vc))
      M <- 1
      Vimp <- 0
      varm <- NULL
      coefm <- NULL
    } # end else for if(varMethod == "j")
  } # end else for if(pvy)
  
  # 7) form output, including final variance estimation
  V <- Vjrr + Vimp
  names(coef) <- names(coef(lm0))
  se <- sqrt(V)
  names(se) <- names(coef)
  coefmat <- data.frame(coef=coef,
                        se=se,
                        t=coef/se)
  
  pti <- pt(coefmat$t, df=length(wgtl$jksuffixes))
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
              njk=njk, varMethod=varmeth)
  res <- c(res, list(n0=nrow2.edsurvey.data.frame(data), nUsed=nrow(edf)))
  class(res) <- "edsurveyLm"
  res
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

#' @method summary edsurveyLm
#' @export
summary.edsurveyLm <- function(object, ...) {
  class(object) <- "summary.edsurveyLm"
  object
}

#' @method summary edsurveyLmList
#' @export
summary.edsurveyLmList <- function(object, ...) {
  class(object) <- "summary.edsurveyLmList"
  for(i in 1:length(object)) {
    class(object[[i]]) <- "summary.edsurveyLm"
  }
  object
}

#' @method print summary.edsurveyLm
#' @importFrom stats printCoefmat
#' @export
print.summary.edsurveyLm <- function(x, ...) {
  cat(paste0("\nFormula: ", paste(deparse(x$formula), collapse=""),"\n\n"))
  if(x$npv > 1) {
    cat(paste0("Plausible values: ", x$npv, "\n"))
    if(x$varMethod %in% "jackknife") {
      cat(paste0("jrrIMax: ", x$jrrIMax, "\n"))
    }
  }
  cat(paste0("Weight variable: ", sQuote(x$weight), "\n"))
  cat(paste0("Variance method: ",x$varMethod,"\n"))
  if(!is.na(x$njk)) {
    cat(paste0("JK replicates: ", x$njk, "\n"))
  }
  cat(paste0("full data n: ", x$n0, "\n"))
  cat(paste0("n used: ", x$nUsed, "\n\n"))
  cat(paste0("Coefficients:\n"))
  printCoefmat(x$coefmat, P.values=TRUE, has.Pvalue=TRUE)
  cat("\n")
  cat(paste0("Multiple R-squared:", round(x$r.squared,4), "\n\n"))
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

# @export
setMethod("lm",
          c(data="edsurvey.data.frame"),
          lm.sdf)

# @export
setMethod("coef",
          c(object="edsurveyLm"),
          coef.edsurveyLm)
