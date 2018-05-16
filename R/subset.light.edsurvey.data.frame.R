# @author Paul Bailey
#' @method [ light.edsurvey.data.frame
#' @export
"[.light.edsurvey.data.frame" <- function(x,i, ...) {
  # do the subset like it's a data.frame
  res <- NextMethod("[")
  if(is.vector(res)) {
    return(res)
  }
  # copy over all of the attributes
  atrs <- names(attributes(x))
  # but don't coppy these attributs over
  atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
  # copy the attributes over
  for(z in atrs) {
    attr(res, z) <- attr(x,z)
  }
  if(inherits(res, "data.frame")) {
    class(res) <- class(x)
  }
  res
}

# @author Trang Nguyen and Paul Bailey
#' @method subset light.edsurvey.data.frame
#' @export
subset.light.edsurvey.data.frame <- function(x, subset, ..., inside=FALSE) {
  env <- parent.frame(n=2)
  
  if(!inherits(x, c("light.edsurvey.data.frame"))) {
    stop(paste0("The argument ", sQuote("x"), " must be a light.edsurvey.data.frame"))
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
  
  r <- eval(condition_call, x)
  res <- x[r,,drop=FALSE]
  return(res)
  
}
