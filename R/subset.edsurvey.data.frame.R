#' @title Subset the rows of an \code{edsurvey.data.frame}.
#'
#' @description Return a new \code{edsurvey.data.frame} which, when passed to
#'              \code{getData}, returns only a subset of the data.
#'
#' @param x an \code{edsurvey.data.frame}.
#' @param subset a condition, written out similar to \code{summary}; when
#'                  \code{inside=TRUE}, parsed text.
#' @param inside set to \code{TRUE} to prevent \code{substitute} 
#'               from being called on the \code{subset} argument, see Details.
#' @param ... not used. Included only for compatability.
#' @details Note that any variables defined in condition that are not references
#' to column names on the
#' \code{edsurvey.data.frame} and are part of the environment where
#' \code{subset.edsurvey.data.frame} was called will be evaluated
#' in the environment from which \code{subset.edsurvey.data.frame} was called.
#' Similar to the difficulty of using subset within a function call because of
#' the call to substitute on condition,
#' this function is difficult to use (with inside set to the default value of
#' \code{FALSE}) inside another function call.
#' See examples for how to call this function from within another function.
#'
#' @return An object of the same class as \code{x} (an \code{edsurvey.data.frame} or
#'         a \code{light.edsurvey.data.frame}).
#' @references
#' Wickham, H. (2014). \emph{Advanced R}. Boca Raton, FL: Chapman & Hall/CRC.
#' 
#' @author Paul Bailey
#' @example man\examples\subset.edsurvey.data.frame.R
#'
#' @method subset edsurvey.data.frame
#' @export
subset.edsurvey.data.frame <- function(x, subset, ..., inside=FALSE) {
  env <- parent.frame(n=2)

  if(!inherits(x, c("edsurvey.data.frame", "light.edsurvey.data.frame"))) {
    stop(paste0(sQuote("x"), " must be an ", sQuote("edsurvey.data.frame")))
  }

  if(inside) {
    condition_call <- subset
  } else {
    # if there is a variable that is not in the data.frame, substitute any
    # value found in the parent.frame() right now.
    # This way, if the user adjusts a variable used in the subset, it will
    # have the value they would have expected from
    # when they called subset and the condition will not change as that
    # variable is updated.
    # add it to the user conditions

    # parse the subset
    # substitute in variables that are available in the current environment
    condition_call <- substitute(subset)
    iparse <- function(ccall,env) {
      # for each element
      for(i in 1:length(ccall)) {
        # if it is a name
        if(class(ccall[[i]]) %in% c("name")) {
          ccall_c <- as.character(ccall[[i]])
          # if it is not in the data and is in the parent.frame, then substitue it now.
          if((! ccall_c %in% names(x$data)) & (ccall_c %in% ls(envir=env)) ) {
            if (ccall[[i]] == "%in%" || is.function(ccall[[i]])) {
              ev <- eval(substitute(ccall[[i]]), parent.frame())  
            } else {
              ev <- eval(ccall[[i]], parent.frame())
            } #end of if/esle statment: if ccall[[i]] == "%in%" || is.function(ccall[[i]])
            ccall[[i]] <- ev
          } # End of if statment: if (! ccall_c %in% names(x$data)) & (ccall_c %in% ls(envir=env)) 
        } # end if(class(ccall[[i]]) %in% c("name")) {
        if(class(ccall[[i]]) %in% "call") {
          # if this is a call, recursively parse that
          ccall[[i]] <- iparse(ccall[[i]], env)
        } #end of if statment: if class(ccall[[i]]) %in% "call"
      } # end of for loop: i in 1:length(ccall)
      ccall
    } # End of fucntion: iparse
    condition_call <- iparse(condition_call, env)
  } # Enf of if esle statmet: if imside is true 

  if( inherits(x, c("light.edsurvey.data.frame")) ) {
    r <- eval(condition_call, x)
    return(x[r,,drop=FALSE])
  }
  # this filter is applied when getData is called
  x[["userConditions"]] <- unique(c(x[["userConditions"]],list(condition_call)))
  x
} # end of fuction subset.edsurvey.data.frame

#' @rdname subset.edsurvey.data.frame
#' @export
setMethod("subset",
          c(x="edsurvey.data.frame"),
          subset.edsurvey.data.frame)

#' @rdname subset.edsurvey.data.frame
#' @export
setMethod("subset",
          c(x="light.edsurvey.data.frame"),
          subset.edsurvey.data.frame)

