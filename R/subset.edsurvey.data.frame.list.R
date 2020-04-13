# @author Trang Nguyen and Paul Bailey
#' @method subset edsurvey.data.frame.list
#' @export
subset.edsurvey.data.frame.list <- function(x, subset, inside=FALSE, drop = FALSE, ...) {
  if(!inherits(x, c("edsurvey.data.frame.list"))) {
    stop(paste0("The argument ",sQuote("x"), " must be an ", dQuote("edsurvey.data.frame.list"), "."))
  }

  if(inside) {
    if(inherits(subset, "character")) {
      subset <- parse(text=subset)[[1]]
    }
    subset_call <- subset
  } else {
    # if there is a variable that is not in the data.frame, substitute any
    # value found in the parent.frame() right now.
    # This way, if the user adjusts a variable used in the subset, it will
    # have the value they would have expected from
    # when they called subset and the subset will not change as that
    # variable is updated.
    # add it to the user subsets

    # parse the subset
    # substitute in variables that are available in the current environment
    subset_call <- substitute(subset)
    iparse <- function(ccall,env) {
      # for each element
      for(i in 1:length(ccall)) {
        # if it is a name
        if(class(ccall[[i]]) %in% c("name")) {
          ccall_c <- as.character(ccall[[i]])
          # if it is not in the data and is in the parent.frame, then substitue it now.
          if((! ccall_c %in% unlist(colnames(x))) & (ccall_c %in% ls(envir=env)) ) {
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
    subset_call <- iparse(subset_call, parent.frame())
  } # Enf of if esle statmet: if inside is true 

  res <- x
  subsetVars <- all.vars(subset_call)
  res$datalist <- lapply(1:length(x$datalist), function(i) {
    li <- x$datalist[[i]]
    # check whether the variable exists the edsurvey.data.frame
    for (v in subsetVars) {
      if (!v %in% colnames(li)) {
        warning(paste0("Variable ", sQuote(v), "is not found in the data ",sQuote(x$covs[i,]),"."))
        return(NULL)
      }
    }
    li[["userConditions"]] <- c(li[["userConditions"]],list(subset_call))
    li
  })
  
  # Remove NULL element
  if (drop) {
    index_removed <- which(sapply(res$datalist,
                                  function(i) {
                                    return(is.null(i) || nrow(i) == 0) }))
  } else {
    index_removed <- which(sapply(res$datalist, is.null))
  }
 
 
  if (length(index_removed) > 0) {
    res$datalist[index_removed] <- NULL
    res$covs <- res$covs[-index_removed,names(res$covs),drop=FALSE]
    row.names(res$covs) <- NULL
  }
  
  # if there is no element left
  if (length(res$datalist) == 0) {
    res <- NULL
  }
  if (length(res$datalist) == 1) {
    res <- res$datalist[[1]]
  }
  res
} # end of fuction subset.edsurvey.data.frame
