#' @title Dimensions of an edsurvey.data.frame or an edsurvey.data.frame.list
#' 
#' @description Returns the dimensions of an \code{edsurvey.data.frame} or an
#'              \code{edsurvey.data.frame.list}.
#'
#' @param x an \code{edsurvey.data.frame} or an \code{edsurvey.data.frame.list}
#' @return For an \code{edsurvey.data.frame}, returns a 
#'         numeric vector of length two, with the first element being the number
#'         of rows and the second element being the number of columns.
#'         
#'         For an \code{edsurvey.data.frame.list}, returns a list of length 
#'         two, where the first list element is named \code{nrow} and is a 
#'         numeric vector containing the number of rows for each element of the 
#'         \code{edsurvey.data.frame.list}. The second element is named
#'         \code{ncol} and is the number of columns for each element.
#'         This is done so that the \code{nrow} and \code{ncol} functions
#'         return meaningful results, even if nonstandard.
#'
#' @author Paul Bailey
#' @aliases dim.edsurvey.data.frame.list
#' @export
dim.edsurvey.data.frame <- function(x) {
  # if there is teacher data
  if(!is.null(x$dataTch)){
    suppressWarnings(data <- getData(x,
                                     varnames=c(names(x$dataTch)[!names(x$dataTch) %in% c(names(x$data), names(x$dataSch))][1]), #we need a unique dataTch variable for this to work correctly
                                     drop=TRUE,
                                     omittedLevels=FALSE,
                                     defaultConditions=FALSE))
  } else {
    suppressWarnings(data <- getData(x,
                                     varnames=c(names(x$data)[1]),
                                     drop=TRUE,
                                     omittedLevels=FALSE,
                                     defaultConditions=FALSE))
  }
  
  nrow <- length(data)
  # every column must have a name
  ncol <- length(colnames(x))
  return(c(nrow, ncol))
}

#' @export
dim.edsurvey.data.frame.list <- function(x) {
  res <- sapply(x$data, function(li) {
    if(is.null(li)) {
     c(NA,NA) 
    } else { 
      dim(li)
    }
  })
  return(list(nrow=res[1,], ncol=res[2,]))
}

# this method used internally to get the number of rows on the original data
nrow2.edsurvey.data.frame <- function(x) {
  getAttributes(x, "dim0")[1]
}


# dimnames (for rownames and colnames)
#' @author Trang Nguyen
#' @export
dimnames.edsurvey.data.frame <- function(x) {
  nameVals <- c()
  if(!is.null(x$data)){
    nameVals <- names(x$data)
  }
  if(!is.null(x$dataSch)){
    nameVals <- c(nameVals, names(x$dataSch))
  }
  if(!is.null(x$dataTch)){
    nameVals <- c(nameVals, names(x$dataTch))
  }
  return(list(NA,unique(nameVals)))
}

#' @author Trang Nguyen
#' @export
dimnames.edsurvey.data.frame.list <- function(x) {
  ret <- lapply(x$datalist, function(li) {
    dimnames(li)[[2]]
  })
  return(list(NA, ret))
}
