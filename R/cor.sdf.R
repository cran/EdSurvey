#' @title Run a bivariate correlation on an edsurvey.data.frame.
#'
#' @description Computes the variance of x and correlation of x and y if these are vectors using an \code{edsurvey.data.frame}.
#'
#' @param x          a character variable name of class factor from the \code{data} to be correlated with y.
#' @param y          a character variable name of class factor from the \code{data} to be correlated with x.
#' @param data        an \code{edsurvey.data.frame}.
#' @param method     a character string indicating which correlation coefficient (or covariance) is to be computed.
#'                   One of \dQuote{Pearson} (default), \dQuote{Spearman}, \dQuote{Polychoric}, or \dQuote{Polyserial}.
#' @param weightVar  character indicating the weight variable to use; see Details.
#' @param reorder    a list to reorder variables. Defaults to \code{NULL}. Can be set as 
#'                   \code{reorder} \code{=} \code{list(var1} \code{=} \code{c("a","b","c"),} \code{var2} \code{=} \code{c("4", "3", "2", "1"))}. See Examples.
#' @param schoolMergeVarStudent a character variable name from the student file used to merge student and school data files. Set to \code{NULL} by default.
#' @param schoolMergeVarSchool a character variable name name from the school file used to merge student and school data files. Set to \code{NULL} by default.
#' @param omittedLevels a logical value. When set to the default value of \code{TRUE}, drops those levels of all factor variables that are specified
#'                        in \code{edsurvey.data.frame}. Use \code{print} on an \code{edsurvey.data.frame} to see the omitted levels.
#' @param defaultConditions a logical value. When set to the default value of \code{TRUE}, uses the default conditions stored in \code{edsurvey.data.frame}
#'                           to subset the data. Use \code{print} on an \code{edsurvey.data.frame} to see the default conditions.
#' @param recode       a list of lists to recode variables. Defaults to \code{NULL}. Can be set as
#'                    \code{recode} \code{=} \code{list(var1} \code{=} \code{list(from} \code{=} \code{c("a","b","c"), to} \code{=} \code{"d"))}. See Examples.
#' @details 
#' Note that the \code{\link{getData}} arguments and \code{recode} may be useful. (See examples.)
#' The correlation methods are calculated as described in seperate documentation.
#' @return
#' An \code{edsurvey.cor} that has print and summary methods.
#'
#' The class includes the following elements:
#' \item{\code{correlation}}{The estimated correlation coefficient.}
#' \item{\code{Zse}}{Square root of the variance (\code{Vimp} + \code{Vjrr}).}
#' \item{\code{correlates}}{A vector of length two showing the columns for which the correlation coefficient was calculated.}
#' \item{\code{variables}}{\code{correlates} that are discrete.}
#' \item{\code{order}}{A list that shows the order of each variable.}
#' \item{\code{method}}{The type of correlation estimated.}
#' \item{\code{Vjrr}}{The jackknife component of variance estimate.}
#' \item{\code{Vimp}}{The imputation component of the variance estimate.}
#' \item{\code{weight}}{The weight variable used.}
#' \item{\code{npv}}{The number of plausible values used.}
#' \item{\code{njk}}{The number of jackknife replicates used.}
#' 
#' @seealso \ifelse{latex}{\code{cor}}{\code{\link[stats]{cor}}} and \ifelse{latex}{\code{weightedCorr}}{\code{\link[wCorr]{weightedCorr}}}
#' @author Paul Bailey. Relies heavily on the \code{wCorr} package, written by Ahmad Emad and Paul Bailey.
#'
#' @example man/examples/cor.sdf.R
#' @importFrom wCorr weightedCorr
#' @importFrom stats var
#' @importFrom stats qt
#' @export
cor.sdf <- function(x,
                    y,
                    data,
                    method = c("Pearson", "Spearman", "Polychoric", "Polyserial"),
                    weightVar = "default",
                    reorder = NULL,
                    schoolMergeVarStudent=NULL,
                    schoolMergeVarSchool=NULL,
                    omittedLevels=TRUE, 
                    defaultConditions=TRUE,
                    recode = NULL) {
  call <- match.call()
  vars <- c(x,y)
  # test input
  checkDataClass(data, c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))

  if(inherits(data[[x]],"character")) {
    stop(paste0("The argument ",sQuote("x"), " must be of a class numeric, factor, or lfactor."))
  }
  if(inherits(data[[y]],"character")) {
    stop(paste0("The argument ",sQuote("y"), " must be of a class numeric, factor, or lfactor."))
  }
  if(nrow(data) <= 0) {
    stop(paste0(sQuote("data"), " must have more than 0 rows."))
  } 

  # test if x and y return TRUE for hasPlausibleValues
  hpvx <- hasPlausibleValue(x, data)
  hpvy <- hasPlausibleValue(y, data)

  # get weight variable
  if(is.null(weightVar)) {
    weightVar <- "one"
    wgt <- "one"
  } else {  # if the weights are not null
    if(weightVar=="default") { # if weightsVar is default
      wgt <- attributes(getAttributes(data, "weights"))$default
    } else { # if weightVar is not defaut
      wgt <- weightVar
    } # End of nested if statment: if weightVar is defalt
  } # End of if statment: if weightVar is null
  
  # setup JK replicate weight variables
  if(weightVar=="one") {
    wgtl <- list(jkbase="one",jksuffixes="")
  } else { # if weightVar is not "one"
    wgtl <- getAttributes(data, "weights")[[wgt]]
  } # End of if statment: if weightVar is "one" 
  wgts <- c(paste0(wgtl$jkbase, wgtl$jksuffixes))
  
  if(weightVar=="one") { 
    getV <- c(vars)
  } else { # if weightVar is not "one"
    getV <- c(vars, wgt)
  } # end of if statment: if weightVar is "one"
  # we should figure out if default conditions was set and pass the warning if it was

  if(missing(defaultConditions)) {
    # do not pass defaultConditions if it was not in the function call
    suppressWarnings(lsdf <- getData(data,
                                     getV,
                                     includeNaLabel=TRUE,
                                     addAttributes=TRUE,
                                     returnJKreplicates=TRUE,
                                     drop=FALSE,
                                     schoolMergeVarStudent=schoolMergeVarStudent,
                                     schoolMergeVarSchool=schoolMergeVarSchool,
                                     omittedLevels=omittedLevels,
                                     recode = recode,
                                     dropUnusedLevels=TRUE))
  } else {
    # pass defaultConditions if it was in the function call
    suppressWarnings(lsdf <- getData(data,
                                     getV,
                                     includeNaLabel=TRUE,
                                     addAttributes=TRUE,
                                     returnJKreplicates=TRUE,
                                     drop=FALSE,
                                     schoolMergeVarStudent=schoolMergeVarStudent,
                                     schoolMergeVarSchool=schoolMergeVarSchool,
                                     omittedLevels=omittedLevels,
                                     defaultConditions=defaultConditions,
                                     recode = recode,
                                     dropUnusedLevels=TRUE))
  } # End of if statment: if defalutConditions were not passed. 

  # if the weight variable is "one" then that will need to be a valid column
  if(weightVar=="one") {
    lsdf$one <- 1
  } 
  
  # do reordering of variables
  if(!is.null(reorder[[x]])) {
    llx <- unique(lsdf[,x])
    if(is.factor(lsdf[,x])) {
      llx <- levels(llx)
    } # End if statment: if lsdf is a factor
    if( sum(!reorder[[x]] %in% llx) > 0 ) {
      bad <- reorder[[x]][!reorder[[x]] %in% llx]
      stop(paste0("Could not find reorder level(s) {", paste(sQuote(bad), collapse=", "), "} when reordering ", sQuote("x"))) 
    } # End if Statment: if sum(!reorder[[x]] %in% llx) > 0
    lsdf[,x] <- factor(lsdf[,x], levels = c(reorder[[x]]))
  } # End if statment: if reorder x is not null 

  # now reorder for y variables
  if(!is.null(reorder[[y]])) {
    lly <- unique(lsdf[,y])
    if(is.factor(lsdf[,y])) {
      lly <- levels(lly)
    } # End if statment: if lsdf[,y] is a factor 
    if( sum(!reorder[[y]] %in% lly) > 0 ) {
      bad <- reorder[[y]][!reorder[[y]] %in% lly]
      stop(paste0("Could not find reorder level(s) {", paste(sQuote(bad), collapse=", "), "} when reordering ", sQuote("y"))) 
    } # End if statment sum(!reorder[[y]] %in% lly) > 0 
    lsdf[,y] <- factor(lsdf[,y], levels = c(reorder[[y]]))
  } # End if statment: if reorder[[y]] is null

  # start levels for variables
  varOrder <- list()
  variables <- c()
  if(length(levels(lsdf[[x]])) || length(levels(lsdf[[y]])) > 0) {
    nums <- !sapply(lsdf, is.numeric)
    variables <- subset(names(nums), nums %in% TRUE)
    for(i in unique(variables)) {
      varn <- c()
      newv <- rep(NA,nrow(lsdf))
      for(z in 1:length(levels(lsdf[[i]]))) {
        varlev <- levels(lsdf[[i]])[z]
        varm <- paste0(z,". ", varlev)
        varn <- c(varn, varm)
        newv[lsdf[[i]] == varlev] <- z
      }
      lsdf[[i]] <- newv
      varlist <- list(c(varn))
      varOrder[i] <-varlist
    }
    names(varOrder)<-c(variables)
  } else { # end if(length(levels(lsdf[[x]])) || length(levels(lsdf[[y]])) > 0) {
    variables <- NULL
    varOrder <- NULL
  } 
  # end reorder variables

  # extract plausible values
  if(hpvx) {
    xvarlsdf <- lsdf[,pvx <- getPlausibleValue(x, lsdf), drop=FALSE] #lsdf
  } else {
    xvarlsdf <- lsdf[,pvx <- x, drop=FALSE] #lsdf
  }
  if(hpvy) {
    yvarlsdf <- lsdf[,pvy <- getPlausibleValue(y, lsdf), drop=FALSE] #lsdf
  } else{
    yvarlsdf <- lsdf[,pvy <- y, drop=FALSE] #lsdf
  }
  
  # drop NAs
  lsdf <- lsdf[!is.na(xvarlsdf[,1]),]
  lsdf <- lsdf[!is.na(yvarlsdf[,1]),]

  # get method
  method <- method[[1]]
  # correlation method (pm)
  pm <- pmatch(tolower(method), tolower(c("Pearson", "Spearman", "Polychoric", "Polyserial")))
  # number of PVs
  npv <- max(npvx <- length(xvarlsdf), npvy <- length(yvarlsdf))
  # the results, across the PVs
  diagVar <- vector(length=npv)
  for(i in 1:npv) {
    if(pm %in% c(1,2)) {
      # Pearson or Spearman
      diagVar[i] <- weightedCorr(xvarlsdf[,min(i,npvx)],yvarlsdf[,min(i,npvy)], method=method, weights=lsdf[,wgt], fast=TRUE, ML=FALSE)
    } 
    if(pm %in% c(3)) {
      # polychoric
      diagVar[i] <- weightedCorr(xvarlsdf[,min(i,npvx)],yvarlsdf[,min(i,npvy)], method="polychoric", weights=lsdf[,wgt], fast=TRUE, ML=FALSE)
    }
    if(pm %in% c(4)) {
      # polyserial
      diagVar[i] <- weightedCorr(xvarlsdf[,min(i,npvx)],yvarlsdf[,min(i,npvy)], method="polyserial", weights=lsdf[,wgt], fast=TRUE, ML=FALSE)
    }
  }

  # for Pearson we will do a forward transform and inverse transform
  # to keep the code simple we simply set the transofmr an intervse transform
  # to the identity function for everything that is not Pearson
  trans <- if(pm==1) {
    atanh
  } else {
    function(x) { x }
  } 
  itrans <- if(pm==1) {
    tanh
  } else {
    function(x) { x }
  } 
  #fisherTransformation for Pearson
  ft <- trans(diagVar)

  # estimated correlation coefficient
  mcc <- itrans(premcc <- mean(ft))

  # the rest of the code estimates the variance

  # for each jackknife replicate
  jkwgtdf <- lsdf[,paste0(wgtl$jkbase, wgtl$jksuffixes), drop=FALSE] #dataframe of jk replicate weights
  wgtdf <- lsdf[,wgts, drop=FALSE] #dataframe of jk replicate weights
  diagVarWgt <- matrix(NA,ncol=length(xvarlsdf),nrow=length(wgts))
  
  # rerun with JK replicate weights
  for(jki in 1:length(wgts)) {
    for(i in 1:npv) {
      if(pm %in% c(1,2)) {
        diagVarWgt[jki,i] <- (trans(weightedCorr(xvarlsdf[,min(i,npvx)],yvarlsdf[,min(i,npvy)], method=method, weights=jkwgtdf[,jki], fast=TRUE, ML=FALSE)) - ft[i])^2
      }
      if(pm %in% c(3)) {
        diagVarWgt[jki,i] <- (trans(weightedCorr(xvarlsdf[,min(i,npvx)],yvarlsdf[,min(i,npvy)], method="polychoric", weights=jkwgtdf[,jki], fast=TRUE, ML=FALSE)) - ft[i])^2
      }
      if(pm %in% c(4)) {
        diagVarWgt[jki,i] <- (trans(weightedCorr(xvarlsdf[,min(i,npvx)],yvarlsdf[,min(i,npvy)], method="polyserial", weights=jkwgtdf[,jki], fast=TRUE, ML=FALSE)) - ft[i])^2
      }
    } # End of for statment i in 1:npv
  } # End of for statment jki in 1:length(wgts)
  # end levels for variables

  # one Vjrr per PV
  preVjrr = apply(diagVarWgt, 2, sum)
  Vjrr = mean(preVjrr)
  # then Vimp = ((M+1)/M) * [variance of main estimate (mcc) across the PVs.]
  # M= number of plausible values
  M <- length(diagVar)
  Vimp <- ifelse( M>1, (M+1)/M * var(ft), 0)
  # then get V
  V <- Vimp + Vjrr
  # but now all of this is in Z-scale, so we have to transform it back.
  Zse <- sqrt(V)
  # do CIs

  if(weightVar=="one") {
    cor <- list(correlation=mcc, Zse=NA, correlates=vars, variables=variables, order=varOrder, method=method,
                Vjrr=NA, Vimp=NA, weight="unweighted", npv=M, njk=NA)
  } else { # else if weightVar is not "one"
    cor <- list(correlation=mcc, Zse=Zse, correlates=vars, variables=variables, order=varOrder, method=method,
                Vjrr=Vjrr, Vimp=Vimp, weight=weightVar, npv=M, njk=length(wgtl$jksuffixes))
  } # End of if statment: if weightVar is "one"
  cor <- c(cor, list(n0=nrow2.edsurvey.data.frame(data), nUsed=nrow(lsdf)))

  class(cor) <- "edsurveyCor"
  cor
}


#' @method print edsurveyCor
#' @export
print.edsurveyCor <- function(x, digits = getOption("digits"), ...) {
  class(x) <- "list"
  cat(paste0("Method: ", x$method, "\n"))
  cat(paste0("full data n: ", x$n0, "\n"))
  cat(paste0("n used: ", x$nUsed, "\n"))
  cat("\n")
  cat(paste0(paste0("Correlation: ", signif(x$correlation, digits=digits), collapse=""), "\n\n"))
  if(length(x$order)>0) {
    cat("\nCorrelation Levels:\n")
    for(var in 1:length(x$order)) {
      cat(paste0("  Levels for Variable '",x$variables[[var]], "' (Lowest level first):\n"))
      for(i in 1:length(x$order[[var]])) {
        cat(paste0("    ",x$order[[var]][i], "\n"))
      } # end of i in 1:length(x$order[[var]])
    } # End of for(var in 1:length(x$order))
  } # End of length(x$order)>0
}
