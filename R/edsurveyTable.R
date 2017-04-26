#' @title Make a table with a edsurvey.data.frame.
#'
#' @description \code{edsurveyTable} returns a summary table (as a \ifelse{latex}{\code{data.frame}}{\code{\link[base]{data.frame}}}) that shows the number
#' of students, the percentage of students, and the mean value of the outcome (or left hand side) variable by the
#' predictor (or right hand side) variable(s).
#'
#' @param formula object of class \ifelse{latex}{\code{formula}}{\code{\link[stats]{formula}}}, potentially with
#'                a subject scale or subscale
#'                on the left hand side, and \dQuote{by variable(s)} for tabulation
#'                on the right hand side. When the left hand side of the
#'                formula is omitted and \code{returnMeans} is \code{TRUE},
#'                then the default subject scale or subscale is used.
#'                You can find the default composite scale and all subscales
#'                using the function \code{\link{showPlausibleValues}}.
#'                Note that the order of the right hand side variables affects the output.
#' @param data object of class \code{edsurvey.data.frame} (see \code{\link{readNAEP}}
#'       for how to generate an \code{edsurvey.data.frame}).
#' @param weightVar character string indicating the weight variable to use.
#'                   Note that only the name of the
#'                   weight variable needs to be included here, and any
#'                   replicate weights will be automatically included.
#'                   When this argument is \code{NULL}, the function uses the default.
#'                   Use \code{\link{showWeights}} to find the default.
#' @param jrrIMax integer indicating the maximum number of plausible values to
#'                 include when calculating
#'                 the variance term \eqn{V_{jrr}} (see the Details section of
#'                 \code{\link{lm.sdf}} to see the definition of \eqn{V_{jrr}}), the default is \code{Inf} and results in
#'                 all available plausible values being used in generating \eqn{V_{jrr}}.
#'                 Setting this to 1 will make code execution faster but less accurate.
#' @param pctAggregationLevel the percentage variable sums up to 100 for the first
#'                              \code{pctAggregationLevel} columns.
#'                              So, when set to 0, the \code{PCT} column adds up to one
#'                              across the entire sample.
#'                              When set to 1, the \code{PCT} column adds up to one
#'                              within each level of the first variable on the
#'                              right hand side of the formula, when set to two,
#'                              then the percentage
#'                              adds up to 100 within the interaction of the
#'                              first and second variable, and so on.
#'                              See Examples section.
#' @param returnMeans a logical value. Set to \code{TRUE} (the default) to get the \code{MEAN} and
#'                     \code{SE(MEAN)} columns in the returned table described in the Value section.
#' @param returnSepct set to \code{TRUE} (the default) to get the \code{SEPCT} column in the returned table described in the Value section.
#' @param varMethod  a character set to \dQuote{jackknife} or \dQuote{Taylor} that indicates the variance estimation method
#'                   to be used. Note that \dQuote{Taylor} is supported only for the column \code{SE(MEAN)} and \dQuote{jackknife} is
#'                   always used for the column \code{SE(PCT)}.
#' @param drop a logical value. When set to the default value of \code{FALSE}, when a single column is returned, it is still represented as a \code{data.frame} and is
#'             not converted to a vector.
#' @param schoolMergeVarStudent a character variable name from the student file used to merge student and school data files. Set to \code{NULL} by default.
#' @param schoolMergeVarSchool a character variable name name from the school file used to merge student and school data files. Set to \code{NULL} by default.
#' @param omittedLevels a logical value. When set to the default value of \code{TRUE}, drops those levels of all factor variables that are specified
#'                        in \code{edsurvey.data.frame}. Use \code{print} on an \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions A logical value. When set to the default value of \code{TRUE}, uses the default conditions stored in \code{edsurvey.data.frame}
#'                           to subset the data. Use \code{print} on an \code{edsurvey.data.frame} to see the default conditions.
#' @param recode a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                  \code{recode} \code{=} \code{list(var1} \code{=} \code{list(from} \code{=} \code{c("a", "b", "c"),} \code{to} \code{=} \code{"c"))}. See Examples.
#'
#' @details This method can be used to generate a simple one to \emph{n}-way
#' table with unweighted and weighted \emph{n} values and percentages. It also
#' can calculate the average of the subject scale or subscale for students at
#' each level of the cross-tabulation table. 
#'       
#' A detailed description of all statistics is given in the \dQuote{Statistics}
#' vignette, which you can find by entering
#' \code{vignette("statistics",} \code{package} \code{=} \code{"EdSurvey")} at
#' the R command prompt.
#' 
#' @return A table with the following columns:
#'    \item{RHS levels}{One column for each right hand side variable. Each row regards students who are at the levels shown in that row.}
#'    \item{\code{N}}{ count of the number of students in the survey in the \code{RHS levels}.}
#'    \item{\code{WTD_N}}{the weighted \emph{N} count of students in the survey in \code{RHS levels}.}
#'    \item{\code{PCT}}{the percentage of students at the aggregation level specified by \code{pctAggregationLevel} (see Arguments).
#'                      See the \dQuote{Statistics} vignette section
#' \dQuote{Estimation of weighted percentages} and its first subsection
#' \dQuote{Estimation of weighted percentages when plausible values are not present.}}
#'    \item{\code{SE(PCT)}}{the standard error of the percentage, accounting for the survey sampling methodology. When \code{varMethod}
#'                       is set to \dQuote{jackknife,} the calculation of this column is described in the \dQuote{Statistics} vignette section
#' \dQuote{Estimation of the standard error of weighted percentages when plausible values are not present, using the jackknife method.}
#'
#'                       When \code{varMethod} is set to \dQuote{Taylor,} then the calculation of this column is described in
#' \dQuote{Estimation of the standard error of weighted percentages when plausible values are not present, using the Taylor series method.}
#' }
#'    \item{\code{MEAN}}{The mean assessment score for units in the \code{RHS levels}, calculated according to the 
#'                       \dQuote{Statistics} vignette section
#' \dQuote{Estimation of weighted means when plausible values are present.}}
#'    \item{\code{SE(MEAN)}}{The standard error of the \code{MEAN} column (the mean assessment score for units in the \code{RHS levels}), calculated according to the 
#'                       \dQuote{Statistics} vignette sections
#' \dQuote{Estimation of standard errors of weighted means when plausible values are present, using the jackknife method}
#' or 
#' \dQuote{Estimation of standard errors of weighted means when plausible values are present, using the Taylor series method,}
#' depending on the value of \code{varMethod}.}
#'
#' @references
#' Binder, D. A. (1983). On the Variances of Asymptotically Normal Estimators From Complex Surveys. \emph{International Statistical Review}, 51(3): 279--92. 
#'
#' Rubin, D. B. (1987). \emph{Multiple Imputation for Nonresponse in Surveys}. New York, NY: Wiley.
#'
#' @example man/examples/edsurveyTable.R
#' @author Paul Bailey and Ahmad Emad
#'
#' @importFrom stats terms
#' @importFrom stats ftable
#' @importFrom stats aggregate
#' @importFrom stats ave
#' @export
edsurveyTable <- function(formula,
                          data,
                          weightVar=NULL,
                          jrrIMax=1,
                          pctAggregationLevel=NULL,
                          returnMeans=TRUE,
                          returnSepct=TRUE,
                          varMethod=c("jackknife", "Taylor"),
                          drop=FALSE,
                          schoolMergeVarStudent=NULL,
                          schoolMergeVarSchool=NULL,
                          omittedLevels=TRUE, 
                          defaultConditions=TRUE,
                          recode=NULL) {
  # Test incoming data 
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  result <- calcEdsurveyTable(formula, data, weightVar, jrrIMax, pctAggregationLevel,
                              returnMeans, returnSepct, varMethod, drop, 
                              schoolMergeVarStudent, schoolMergeVarSchool, omittedLevels,
                              defaultConditions, recode,
                              defaultConditionsMissing=missing(defaultConditions))
  result
}

calcEdsurveyTable <- function(formula,
                              data,
                              weightVar=NULL,
                              jrrIMax=1,
                              pctAggregationLevel=NULL,
                              returnMeans=TRUE,
                              returnSepct=TRUE,
                              varMethod=c("jackknife", "Taylor"),
                              drop=FALSE,
                              schoolMergeVarStudent=NULL,
                              schoolMergeVarSchool=NULL,
                              omittedLevels=TRUE, 
                              defaultConditions=TRUE,
                              recode=NULL,
                              defaultConditionsMissing=TRUE
) {
  # test incoming data
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))
  
  if(is.null(weightVar)) {
    wgt <- attributes(getAttributes(data, "weights"))$default
  } else {
    wgt <- weightVar
  }
  
  varMethod <- tolower(varMethod[[1]])
  if(!varMethod %in% substr(c("jackknife", "taylor"),0,nchar(varMethod)) ) {
    stop(paste0("The argument ",sQuote("varMethod"), " must be one of ", dQuote("jackknife"), " or ", dQuote("Taylor"), "."))
  }
  varMethod <- substr(varMethod, 0,1)
  
  # fill in default subject scale / sub scale
  zeroLengthLHS <- attr(terms(formula), "response") == 0
  
  # get y variables
  if( zeroLengthLHS ) {
    yvar <- attributes(getAttributes(data, "pvvars"))$default
    formula <- formula(paste0(yvar, " ", paste(as.character(formula), collapse="")))
  } else{
    yvar <- all.vars(formula[[2]])
  }
  rhs_vars <- all.vars(formula[[3]])
  
  # define default aggregation level
  if(is.null(pctAggregationLevel)) {
    # set this to aggregate as far down as is reasonable
    pctAggregationLevel <- length(rhs_vars) - 1
  }
  # if it is not possible, reset it.
  if(pctAggregationLevel > length(rhs_vars) - 1) {
    pctAggregationLevel <- length(rhs_vars) - 1
    warning(paste0("Resetting ", sQuote("pctAggregationLevel"), " to ", pctAggregationLevel, ", the largest potentially meaningful value."))
  }
  
  if(length(yvar) > 1  & returnMeans) {
    stop("There must be exactly one left hand side variable in the ", sQuote("formula")," argument.")
  }
  
  # get PVs
  pvy <- hasPlausibleValue(yvar, data)
  yvars <- yvar
  if(pvy) {
    yvars <- getPlausibleValue(yvar, data)
  } else {
    if(!yvar %in% names(data)) {
      stop(paste0("Cannot find ", sQuote(yvars), " in the edsurvey.data.frame."))
    }
  }
  
  if(returnMeans) {
    yvar0 <- yvars[1]
  }
  
  #We need to get all the weights as well
  wgtl <- data$weights[[wgt]]
  wgtall <- c()
  for(jki in 1:length(wgtl$jksuffixes)) {
    wgti <- paste0(wgtl$jkbase, wgtl$jksuffixes[jki])
    wgtall <- c(wgtall, wgti)
  }
  reqvar <- c(all.vars(formula), wgt)
  if(returnMeans) {
    reqvar <- c(reqvar, wgtall)
  }
  # this is necessary because the LHS variable was always missing when the RHS variables was NA
  includeNaLabel <- !returnMeans
  # add Taylor variables
  if(varMethod=="t") {
    reqvar <- c(reqvar, getAttributes(data, "psuVar"), getAttributes(data, "stratumVar"))
  }
  # only call with defaultConditions if it was in the call to edsurveyTable
  if(defaultConditionsMissing) {
    edf  <- getData(data, reqvar, includeNaLabel=includeNaLabel,
                    returnJKreplicates=(varMethod=="j" & (returnMeans | returnSepct)), dropUnusedLevels=TRUE, 
                    drop= drop, schoolMergeVarStudent=schoolMergeVarStudent, schoolMergeVarSchool=schoolMergeVarSchool,
                    omittedLevels=omittedLevels, recode=recode, addAttributes=TRUE)
  } else {
    edf  <- getData(data, reqvar, includeNaLabel=includeNaLabel,
                    returnJKreplicates=(varMethod=="j" & (returnMeans | returnSepct)), dropUnusedLevels=TRUE, 
                    drop= drop, schoolMergeVarStudent=schoolMergeVarStudent, schoolMergeVarSchool=schoolMergeVarSchool,
                    omittedLevels=omittedLevels, defaultConditions=defaultConditions, recode=recode, addAttributes=TRUE)
  }
  n <- ftable(edf[,rhs_vars, drop=FALSE])
  res <- data.frame(n)
  wtdn <- fastAgg(formula(paste0(wgt, " ~ ", paste(rhs_vars, collapse=" + "))), data=edf,  FUN=sumna)
  
  names(res) <- c(rhs_vars, "N") # does nothing unless length(rhs_vars)==1
  last_column <- names(wtdn)[length(names(wtdn))]
  wtdn$WTD_N <- wtdn[,last_column]
  wtdn[,last_column] <- NULL
  res <- merge(res, wtdn, sort=FALSE)
  if(pctAggregationLevel == 0) {
    res$twt <- sum(res$WTD_N)
    res$group <- 1 # used in Taylor series sePct
  } else {
    twt <- fastAgg(formula(paste0('WTD_N' , " ~ ", paste(rhs_vars[1:pctAggregationLevel], collapse=" + "))), data=res,  FUN=sum)
    names(twt) <- c(rhs_vars[1:pctAggregationLevel],'twt')
    twt$group <- 1:nrow(twt) # used in Taylor series sePct
    res <- merge(res, twt, by=rhs_vars[1:pctAggregationLevel])
  }
  res['PCT'] <- res[,'WTD_N']/res[,'twt']*100
  # get SE on weighted n
  if(returnSepct) {
    wtdnvar <- rep(0, nrow(res))
    wgtl <- getAttributes(data, "weights")[[wgt]]
    if(varMethod == "j") {
      for(jki in 1:length(wgtl$jksuffixes)) {
        wgti <- paste0(wgtl$jkbase, wgtl$jksuffixes[jki])
        wtdn <- fastAgg(formula(paste0(wgti, " ~ ", paste(rhs_vars, collapse=" + "))), data=edf,  FUN=sumna)
        if(pctAggregationLevel==0) {
          wtdt <- aggregate(formula(paste0(wgti, " ~ 1")), data=wtdn, FUN=sum, na.rm=TRUE)
        } else {
          wtdt <- fastAgg(formula(paste0(wgti, " ~ ", paste(rhs_vars[1:pctAggregationLevel], collapse=" + "))), data=wtdn, FUN=sumna)
        }
        names(wtdt)[ncol(wtdt)] <- "twti"
        # last_column is the name of the column with the wgti sum in it
        last_column <- names(wtdn)[length(names(wtdn))] 
        wtdn <- merge(wtdn, res, by=rhs_vars)
        if(pctAggregationLevel==0) {
          wtdn$one__ <- 1
          wtdt$one__ <- 1
          wtdn <- merge(wtdn, wtdt, by="one__")
        } else {
          wtdn <- merge(wtdn, wtdt, by=rhs_vars[1:pctAggregationLevel])
        }
        wtdnvar <- wtdnvar + (wtdn[last_column]/wtdn['twti'] - wtdn["PCT"]/100)^2
      }
      wtdndf <- data.frame(stringsAsFactors=FALSE,
                           100*sqrt(wtdnvar))
      names(wtdndf)[1] <- "SE(PCT)"
      wtdndf[,rhs_vars] <- wtdn[,rhs_vars]
      res <- merge(res, wtdndf, by=rhs_vars, sort=FALSE)
    } else { # Taylor series based method
      res[,"SE(PCT)"] <- NA
      sapply(unique(res$group), function(z) { # for each grouping
        
        resi <- res[res$group == z,]# subset(res, group == z)
        n <- nrow(resi)
        resi$groupsubset <- 1:n
        if(n!=1) { # if n>1 then the percent is not 100 and we need to find SE(PCT)
          pr <- res[res$group==z,"PCT"]/100
          datai <- edf
          if(pctAggregationLevel>0) {
            for(i in 1:pctAggregationLevel) {
              datai$rhsi <- datai[,rhs_vars[i]]
              vvv <- as.character(resi[1,rhs_vars[i]])
              datai <- datai[datai$rhsi == vvv,]
            }
          }
          datai$weight__n__ <- datai[,wgt]
          # identify unit for each obs
          for(i in 1:n) {
            datai$gss <- 1 
            # set gss to 1 for just rows in this group
            for(j in (pctAggregationLevel+1):length(rhs_vars)) {
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
            wtilde[,j] <- datai[,wgt] * pr[j]
          }
          # again using notation from AM documentation, including multiplicaiton by w
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
            uhiw <- aggregate(v ~ psuV + stratV, data=uhijw, FUN=sum)
            uhiw$vv <- ave(uhiw$v, uhiw$stratV, FUN=meanna)
            uhiw$dx <- uhiw$v - uhiw$vv
            nam <- names(uhiw)
            nam[nam=="v"] <- paste0("uhi",vi)
            nam[nam=="vv"]  <- paste0("uj",vi)
            nam[nam=="dx"]  <- paste0("dx",vi)
            names(uhiw) <- nam
            if(vi == 1) {
              uhiw_ <- uhiw
            } else {
              uhiw_ <- merge(uhiw_, uhiw, by=c("stratV", "psuV"))
            }
          }
          repu <- unique(uhiw_$stratV)
          S <- matrix(0, nrow=nrow(resi), ncol=nrow(resi))
          for(repi in 1:length(repu)) {
            dataii <- uhiw_[uhiw_$stratV == repu[repi],]
            jku <- unique(dataii$psuV)
            ni <- length(jku)
            if(ni > 1) {
              for(jkj in 1:ni) {
                vec <- unlist(dataii[dataii$psuV==jku[jkj], paste0("dx",1:n),drop=TRUE])
                S <- S + (ni/(ni-1)) * vec %*% t(vec)
              }
            }
          }
          D <- diag(rep(1/resi$twt[1],nrow(resi)))# * (diag(resi$twt) - cb)
          var <- D %*% S %*% t(D)
          # this is a check mentioned in the statistics vignette, saved for posterity
          #np <- nrow(D)-1
          #Dp <- D[1:np, 1:np]
          #Sp <- S[1:np, 1:np]
          #varp11 <- Dp %*% Sp %*% t(Dp)
          #onep <- rep(1, np)
          #varp12 <- -1* varp11 %*% onep
          #varp21 <- t(varp12)
          #varp22 <- sum(-1*varp12)
          #varp <- rbind(cbind(varp11,varp12), c(varp21,varp22))
          # varp now equals var
          #
          res[res$group==z,"SE(PCT)"] <<- 100 * sqrt(diag(var)) # fit to percentage
        } else { # end if(n!=1) { 
          # there is only one thing in this aggregation level
          # so the percent will be 100. The frequentist SE on this will be zero.
          res[res$group==z,"SE(PCT)"] <<- 0
        } # end else for if(n!=1) { 
      }) # end sapply(unique(res$group), function(z) {
    } # end else for if(varMethod == "j") {
  } # end if(returnSepct) {
  
  
  res['group'] <- NULL
  res['twt'] <- NULL
  res <- res[order(res[,rhs_vars[1]]),]
  
  # added in 0.6.0 unclear why it is needed
  res <- res[res$N>0,]#subset(res, N > 0)
  if(returnMeans) {
    # if lm.sdf had a model.frame method, it could be used here thusly:
    # data <- lm.sdf(formula(paste0(yvar, " ~ 0 + ", paste(rev(rhs_vars)), collapse=":", method="model.frame")), dsdf)
    
    lvls <- levels(edf[,rhs_vars[1]]) # get the levels in the order they came in
    lvls <- lvls[lvls %in% edf[,rhs_vars[1]]] # keep just occupied levels
    ilen <- nrow(res)/length(lvls) # number of values per level of rhs_vars[1]
    for(i in length(rhs_vars):1) {
      res <- res[order(res[,rhs_vars[i]]),]
    }
    # for each row of the table, get the mean and SE from lm
    for(i in 1:nrow(res)) {
      dsdf <- edf
      for(j in 1:length(rhs_vars)) {
        cond <- parse(text=paste0(rhs_vars[j], " == \"", res[i,rhs_vars[j]],"\""))[[1]]
        if(inherits(dsdf, "edsurvey.data.frame")) {
          dsdf <- subset.edsurvey.data.frame(dsdf, cond, inside=TRUE) # subset to just those values at level i of the first X variable
        } else {
          dsdf <- dsdf[dsdf[,rhs_vars[j]] %in% res[i,rhs_vars[j]],] # subset to just those values at level i of the first X variable
        }
      }
      fi <- formula(paste0(yvar, " ~ 1"))
      lst <- list(fi, dsdf,
                  weightVar=wgt, jrrIMax=jrrIMax,
                  varMethod=varMethod,
                  omittedLevels=FALSE) # taken care of above
      lmi <- do.call(lm.sdf, lst)
      cf <- summary(lmi)$coefmat
      if(!is.na(cf$coef)) {
        res[i,"MEAN"] <- cf$coef
        res[i,"SE(MEAN)"] <- cf$se
        lmi <- NULL
      }
    } # end for(i in 1:nrow(res)) {
  } # if(returnMeans)
  njk <- length(wgtl$jksuffixes)
  if(varMethod== "t") {
    njk <- NA
  }
  rownames(res) <- NULL
  varmeth <- ifelse(varMethod=="t", "Taylor series", "Jackknife")
  
  # Add variable labels as an attribute
  if (inherits(data, c("edsurvey.data.frame", "light.edsurvey.data.frame"))){
    for (i in 1:length(names(res))) {
      if (names(res)[i] %in% all.vars(formula)) {
        suppressWarnings(attr(res[[names(res)[i]]], "label") <- searchSDF(names(res)[i], data)$Labels)
      }
    }
  }
  
  res2 <- list(formula=formula, npv=length(yvars),
               jrrIMax=min(jrrIMax, length(yvars)), weight=wgt,
               njk=njk, varMethod=varmeth, data=res)
  res2 <- c(res2, list(n0=nrow2.edsurvey.data.frame(data), nUsed=nrow(edf)))
  class(res2) <- "edsurveyTable" 
  res2
}


#' @title Prints summary details of a summary table from an edsurvey.data.frame.
#'
#' @description Prints summary details of a summary table
#' estimates appropriate for the \code{edsurvey.data.frame}.
#' @param x an R object representing a summary table of an \code{edsurvey.data.frame}.
#' @param digits number of significant figures to print.
#' @param ... these arguments are not passed anywhere and are included only for compatibility.
#' @method print edsurveyTable
#' @author Paul Bailey and Howard Huo
#' @export 
print.edsurveyTable <- function(x, digits=getOption("digits"), ...) {
  cat(paste0("\nFormula: ", paste(deparse(x$formula), "\n",collapse="")))
    
  if(x$npv != 1) {
    # only print if it could be larger than one.
    cat(paste0("\njrrIMax: ", x$jrrIMax, "\n"))
  }
  cat(paste0("Weight variable: ", sQuote(x$weight), "\n"))
  cat(paste0("Variance method: ",x$varMethod,"\n"))
  if(!is.na(x$njk)) {
    cat(paste0("JK replicates: ", x$njk, "\n"))
  }
  if(!is.na(x$n0)) {
    cat(paste0("full data n: ", x$n0, "\n"))
    cat(paste0("n used: ", x$nUsed, "\n\n"))
  }
  cat("\n")
  cat(paste0("Summary Table:\n"))
  print(x$data, digits=digits, row.names=FALSE, ...)
}

sumna <- function(x) { sum(x, na.rm=TRUE)}
