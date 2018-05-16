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
#' @param jrrIMax    when using the jackknife variance estimation method, the \eqn{V_{jrr}} term
#'                   (see Details) can be estimated with
#'                   any positive number of plausible values and is estimated on 
#'                   the lower
#'                   of the number of available plausible values and \code{jrrIMax}. When
#'                   \code{jrrIMax} is set to \code{Inf}, all plausible values will be used.
#'                   Higher values of \code{jrrIMax} lead to longer computing times and more
#'                   accurate variance estimates.
#' @param relevels   a list; used when the user wants to change the contrasts from the
#'                   default treatment contrasts to the treatment contrasts with a chosen omitted
#'                   group. The name of each element should be the variable name, and the value 
#'                   should be the group to be omitted.
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
#'                           estimates. This is intended to allow for
#'                           the computation
#'                           of covariances between estimates.
#' @details 
#'  
#' This function implements an estimator that correctly handles left-hand
#' side variables that are either numeric or plausible values and allows for survey 
#' sampling weights and estimates variances using the jackknife replication method.
#' The
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistics vignette}
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
#'
#' \subsection{Variance estimation of coefficients}{
#'   All variance estimation methods are shown in the
#' \href{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}{Statistics vignette}.
#'   When \code{varMethod} is set to \code{jackknife} and the predicted
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
#'   When plausible values are present and \code{varMethod} is \dQuote{Taylor,} the
#'   variance of the coefficients is estimated according to the section
#' \dQuote{Estimation of Standard Errors of Weighted Means When Plausible
#'         Values Are Present, Using the Taylor Series Method.}
#' }
#'
#' @references
#' Binder, D. A. (1983). On the variances of asymptotically normal estimators from complex surveys. \emph{International Statistical Review}, \emph{51}(3), 279--292. 
#'
#' Rubin, D. B. (1987). \emph{Multiple imputation for nonresponse in surveys}. New York, NY: Wiley.
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
#'    \item{njk}{the number of jackknife replicates used; set to \code{NA} when Taylor series variance estimates are used}
#'    \item{varMethod}{one of \code{Taylor series} or \code{jackknife}}
#'    \item{varEstInputs}{when \code{returnVarEstInputs} is \code{TRUE},
#'                        this element is returned. These are
#'                        used for calculating covariances with
#'                        \code{\link{varEstToCov}}.}
#'
#' @seealso \ifelse{latex}{\code{lm}}{\code{\link[stats]{lm}}}
#' @author Paul Bailey
#'
#' @example \man\examples\lm.sdf.R
#' @importFrom Matrix sparse.model.matrix
#' @importFrom stats lm
#' @importFrom stats aggregate
#' @importFrom stats pt
#' @importFrom stats relevel
#' @importFrom stats model.matrix
#' @importFrom stats lm.wfit
#' @importFrom stats as.formula
#' @importFrom stats complete.cases
#' @importFrom Formula Formula
#' @importFrom MASS ginv
#' @method lm sdf
#' @export
#' @export lm.sdf
#' @usage lm.sdf(formula, data, weightVar = NULL, relevels = list(),
#'               varMethod = c("jackknife", "Taylor"), jrrIMax = 1, omittedLevels = TRUE,
#'               defaultConditions = TRUE, recode = NULL, returnVarEstInputs = FALSE)
lm.sdf <- function(formula,
                   data,
                   weightVar=NULL,
                   relevels=list(),
                   varMethod=c("jackknife", "Taylor"),
                   jrrIMax=1,
                   omittedLevels=TRUE,
                   defaultConditions=TRUE,
                   recode=NULL,
                   returnVarEstInputs=FALSE) {
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
                       call=call))
  }
}

calc.lm.sdf <- function(formula,
                        data,
                        weightVar=NULL,
                        relevels=list(),
                        varMethod=c("jackknife", "Taylor"),
                        jrrIMax=1,
                        omittedLevels=TRUE,
                        defaultConditions=TRUE,
                        missingDefaultConditions=TRUE,
                        recode=NULL,
                        returnVarEstInputs=FALSE,
                        returnLm0=FALSE,
                        call=NULL) {
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
  varMethod <- substr(tolower(match.arg(varMethod)), 0, 1)

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
                   omittedLevels=omittedLevels,
                   recode = recode,
                   includeNaLabel=TRUE,
                   dropUnusedLevels=TRUE)

  # Default conditions should be included only if the user set it. This adds the argument only if needed
  if(!missingDefaultConditions) {
    getDataArgs <- c(getDataArgs, list(defaultConditions=defaultConditions))
  }
  # edf is the actual data
  edf <- do.call(getData, getDataArgs)
  if(any(!complete.cases(edf))) {
    warning("Removing rows with NAs from analysis.")
    edf <- edf[complete.cases(edf),]
  }

  # check that there are not factors with only one level and give more informative error. 
  lapply(names(edf)[names(edf) %in% all.names(formula)],
          function(z) {
            if(is.factor(edf[,z]) & length(levels(edf[,z])) <2) {
              stop(paste0("The covariate ",
                          dQuote(z),
                          " has fewer than two levels and cannot be included in the regression."
                          ))
            }
          }
        )
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
  if(returnLm0) {
    return(lm0)
  }
  # this grabs the list of weight variable names
  wgtl <- getAttributes(sdf, "weights")[[wgt]]
  varEstInputs <- list()
  # note we have not made the B matrix
  madeB <- FALSE
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
    varM <- list()
    # coefficients by PV
    coefm <- matrix(NA, nrow=length(yvars), ncol=length(coef(lm0)))
    # R-squared by PV
    r2s <- vector(length=length(yvars))
    if(varMethod == "j") {
      # y is a variable with plausible values and we are using the jackknife apporach
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
        # if(sum(!is.na(coefa))==0 || sum(coefa,na.rm=TRUE) == 0) {
        #   stop("No variance across strata. This could be due to only one stratum being included in the sample.")
        # }
        frm <- Formula::Formula(frm)
        if(terms(frm, lhs = 0, rhs = NULL) != ~1){
          if(sum(!is.na(coefa))==0 || sum(coefa,na.rm=TRUE) == 0) {
            stop("No variance across strata. This could be due to only one stratum being included in the sample.")
          }
        }
        if(pvi == 1) {
            dfl <- lapply(1:ncol(coefa), function(coli) {
              data.frame(PV=rep(pvi, nrow(coefa)),
                         JKreplicate=1:nrow(coefa),
                         variable=names(coef(lmi))[coli],
                         value=coefa[,coli])
            })
            veiJK <- do.call(rbind, dfl)
          } else {
            dfl <- lapply(1:ncol(coefa), function(coli) {
              data.frame(PV=rep(pvi, nrow(coefa)),
                         JKreplicate=1:nrow(coefa),
                         variable=names(coef(lmi))[coli],
                         value=coefa[,coli])
            })
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
      } # End while loop: pvi < length(yvars)
    } else { # End of if statment: varMethod == "j"
      # Taylor series
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
        for(bi in 1:length(b)) { # for each coefficient
          coln <- colnames(uhij)[bi]
          # get the stratum/PSU based sum
          edf$bb <- uhij[,bi] * edf[,wgt]
          taylorVars <- c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar"))
          res_sp <- aggregate(formula(paste0("bb ~ ", getAttributes(data, "psuVar"), " + ", getAttributes(data, "stratumVar"))), edf, sum)
          # and the average of the same across strata
          res_s <- aggregate(formula(paste0("bb ~ ", getAttributes(data, "stratumVar"))),res_sp,function(x) { mean(x)})
          names(res_sp)[3] <- coln
          names(res_s)[2] <- paste0("uh_",coln)
          if(bi==1) {
            uhi <- res_sp
            edf$strtWgt <- edf[,wgt]
            res_s_wgt <- aggregate(formula(paste0("strtWgt ~ ", getAttributes(data, "stratumVar"))), edf, function(x) { sum(x)})
            uhi <- merge(uhi, res_s_wgt, by=c(getAttributes(data, "stratumVar")), all=TRUE)
          } else{
            uhi <- merge(uhi,res_sp, by=c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar")), all=TRUE)
          }
          uhi <- merge(uhi, res_s, by=c(getAttributes(data, "stratumVar")), all=TRUE)
          uhi[,paste0("dx",bi)] <- uhi[,coln] - uhi[,paste0("uh_",coln)]
        } #end of for(bi in 1:length(b)) 

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
            sw <- (uhi$strtWgt[uhi[,getAttributes(data, "stratumVar")]==ii])[1]
            num <- sw * diag(vvj)
            dofNum[,mm] <<- dofNum[,mm] + num
            dofDenom[,mm] <<- dofDenom[,mm] + num^2
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
      # done getting var est inputs
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
      X <- sparse.model.matrix(frm, edf)
      W <- Diagonal(n=nrow(edf),edf[,wgt,drop=TRUE])
      XW <-X * sqrt(edf[,wgt,drop=TRUE])
      qrXW <- qr(XW)
      D <- solve(t(X) %*% W %*% X)
      Y <- as(edf[,yvars[1],drop=FALSE],"Matrix")
      YW <- Y * sqrt(edf[,wgt,drop=TRUE])

      b <- qr.coef(qrXW, YW)
      fitted <- X%*%b
      e <- as.vector((Y-fitted))
      # this is the partial of the likelihood at the unit level
      uhij <- e*as.matrix(X)
      
      for(bi in 1:length(b)) { # for each coefficient
        coln <- colnames(uhij)[bi]
        # get the stratum/PSU based sum
        edf$bb <- uhij[,bi] * edf[,wgt]
        resi <- aggregate(formula(paste0("bb ~ ", getAttributes(data, "psuVar"), " + ", getAttributes(data, "stratumVar"))), edf, sum)
        # and the average of the same across strata
        resj <- aggregate(formula(paste0("bb ~ ", getAttributes(data, "stratumVar"))),resi,function(x) { mean(x)})
        names(resi)[3] <- coln
        names(resj)[2] <- paste0("uh_",coln)
        if(bi==1) {
          uhi <- resi
          edf$strtWgt <- edf[,wgt]
          resw <- aggregate(formula(paste0("strtWgt ~ ", getAttributes(data, "stratumVar"))), edf, function(x) { sum(x)})
          uhi <- merge(uhi, resw, by=c(getAttributes(data, "stratumVar")), all=TRUE)
        } else{
          uhi <- merge(uhi,resi, by=c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar")), all=TRUE)
        }
        uhi <- merge(uhi, resj, by=c(getAttributes(data, "stratumVar")), all=TRUE)
        uhi[,paste0("dx",bi)] <- uhi[,coln] - uhi[,paste0("uh_",coln)]
      } # End of for loop: bi in 1:length(b)
      # this will be the variance-covariance matrix for this plausible value
      vv <- matrix(0,nrow=length(b), ncol=length(b))
      dofNum <- rep(0,length(b))
      dofDenom <- rep(0,length(b))
      sa <-  lapply(unique(uhi[,getAttributes(data, "stratumVar")]), function(ii) {
        vvj <- matrix(0,nrow=length(b), ncol=length(b))
            unkj <- unique(uhi[uhi[,getAttributes(data, "stratumVar")] == ii, getAttributes(data, "psuVar"), drop=TRUE])
        if(length(unkj)>1) { # cannot estimate variance of single unit
          sb <- lapply(unkj, function(jj) {
            v <- as.numeric(t(uhi[uhi[,getAttributes(data, "stratumVar")]==ii & uhi[,getAttributes(data, "psuVar")]==jj, paste0("dx",1:length(b)), drop=FALSE]))
            vvj <<- vvj + v %*% t(v)
          })
          vvj <- vvj * ( (length(unkj)) / ( length(unkj) - 1) )
          sw <- (uhi$strtWgt[uhi[,getAttributes(data, "stratumVar")]==ii])[1]
          num <- sw * diag(vvj)
          dofNum <<- dofNum + num
          dofDenom <<- dofDenom + num^2
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
  
  # Deal with estimates with no sampling variance, make sure they return NA for SE
  Vjrr[Vjrr == 0] <- NA
  
  V <- Vjrr + Vimp
  names(coef) <- names(coef(lm0))
  se <- sqrt(V)
  names(se) <- names(coef)

  # get fitted and resid based on overall coef
  X <- model.matrix(frm, edf) 
  fitted1 <- as.vector(X%*%coef)
  Y <- sapply(1:length(yvars), function(yi) {
    as.vector(edf[,yvars[yi]])
  }, simplify=TRUE)
  resid1 <- Y - fitted1
  colnames(resid1) <- yvars
  
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
    coefmat$dof <- (3.16 - 2.77/sqrt(m)) * apply(dofNum^2/dofDenom, 1, mean)
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
              residuals=resid1, fitted.values=fitted1)
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
        frm <- Formula::Formula(frm)
        if(terms(frm, lhs = 0, rhs = NULL) == ~1){
          warning("A variance estimate was replaced with NA, because there was no variance across strata.", call. = FALSE)
        } else {
          warning("Variance estimation problematic, consider using jackknife", call. = FALSE)
        }
      }
    )
    Ttilde <- (1+rbar)*Ubar
    res <- c(res, list(B=B, U=Ubar, rbar=rbar, Ttilde=Ttilde))
  }
  res <- c(res, list(n0=nrow2.edsurvey.data.frame(data), nUsed=nrow(edf)))
  if(inherits(data, "edsurvey.data.frame")) {
    res <- c(res, list(data=data))
  } else {
    res <- c(res, list(lm0=lm0))
  }
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
  if(x$npv != 1) {
    cat(paste0("jrrIMax: ", x$jrrIMax, "\n"))
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
