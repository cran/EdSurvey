#' @name subset
#' @title EdSurvey Subset
#' @aliases subset.edsurvey.data.frame.list subset.light.edsurvey.data.frame
#'
#' @description Subsets an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list},
#' or a \code{light.edsurvey.data.frame}.
#'
#' @param x an \code{edsurvey.data.frame}, an \code{edsurvey.data.frame.list},
#'          or a \code{light.edsurvey.data.frame}
#' @param subset a logical expression indicating elements or rows to keep
#' @param inside set to \code{TRUE} to prevent the \code{substitute} condition
#'               from being called on it. See Details.
#' @param ... not used; included only for compatibility.
#' @details Note that any variables defined on condition that are not references
#' to column names on the
#' \code{edsurvey.data.frame} and are part of the environment where
#' \code{subset.edsurvey.data.frame} was called will be evaluated
#' in the environment from which \code{subset.edsurvey.data.frame} was called.
#' Similar to the difficulty of using subset within a function call because of
#' the call to substitute on condition,
#' this function is difficult to use (with inside set to the default value of
#' \code{FALSE}) inside another function call.
#' See Examples for how to call this function from within another function.
#'
#' @return an object of the same class as \code{x}
#' @references
#' Wickham, H. (2014). \emph{Advanced R}. Boca Raton, FL: Chapman & Hall/CRC.
#' 
#' @author Trang Nguyen and Paul Bailey
#' @example man\examples\subset.edsurvey.data.frame.R
#' @method subset edsurvey.data.frame
#' @export
subset.edsurvey.data.frame <- function(x, subset, ..., inside=FALSE) {
  env <- parent.frame(n=2)
  
  if(!inherits(x, c("edsurvey.data.frame"))) {
    stop(paste0("The argument ", sQuote("x"), " must be an edsurvey.data.frame"))
  }

  if(inside) {
    if(inherits(subset, "character")) {
      subset <- parse(text=subset)[[1]]
    }
    condition_call <- subset
  } else {
    # if there is a variable that is not in the data.frame, substitute any
    # value found in the parent.frame() right now.
    # This way, if the user adjusts a variable used in the subset, it will
    # have the value they would have expected from
    # when they called subset and the condition will not change as that
    # variable is updated.
    # add it to the user conditions

    # parse the condition
    # substitute in variables that are available in the current environment
    condition_call <- substitute(subset)
    iparse <- function(ccall,env) {
      # for each element
      for(i in 1:length(ccall)) {
        # if it is a name
        if(class(ccall[[i]]) %in% c("name")) {
          ccall_c <- as.character(ccall[[i]])
          # if it is not in the data and is in the parent.frame, then substitue it now.
          if((! ccall_c %in% colnames(x)) & (ccall_c %in% ls(envir=env)) ) {
            if (ccall[[i]] == "%in%" || is.function(ccall[[i]])) {
              ev <- eval(substitute(ccall[[i]]), parent.frame())  
            } else {
              ev <- eval(ccall[[i]], parent.frame())
            } #end of if/esle statment: if ccall[[i]] == "%in%" || is.function(ccall[[i]])
            ccall[[i]] <- ev
          } # End of if statment: if (! ccall_c %in% colnames(x$data)) & (ccall_c %in% ls(envir=env)) 
        } # end if(class(ccall[[i]]) %in% c("name")) {
        if(class(ccall[[i]]) %in% "call") {
          # if this is a call, recursively parse that
          ccall[[i]] <- iparse(ccall[[i]], env)
        } #end of if statment: if class(ccall[[i]]) %in% "call"
      } # end of for loop: i in 1:length(ccall)
      ccall
    } # End of fucntion: iparse
    condition_call <- iparse(condition_call, env)
    #condition_call <- iparse(condition_call, parent.frame())
  } # Enf of if esle statmet: if imside is true 
  
  # check whether variable exists in the edsurvey.data.frame
  subsetVars <- all.vars(condition_call)
  for (v in subsetVars) {
    if (!v %in% colnames(x)) {
      stop("Variable ", sQuote(v), "is not found in the incoming data.")
    }
  }
  # apply filter
  x[["userConditions"]] <- c(x[["userConditions"]],list(condition_call))
  x
} # end of fuction subset.edsurvey.data.frame

#' @method [ edsurvey.data.frame
#' @export
"[.edsurvey.data.frame" <- function(x, i, ...) {
  vn <- colnames(x)
  suppressWarnings(
    z <- getData(x, varnames=vn,
                 dropUnusedLevels=FALSE, omittedLevels=FALSE,
                 addAttributes=TRUE)
    )
  z[i, ...]
}
