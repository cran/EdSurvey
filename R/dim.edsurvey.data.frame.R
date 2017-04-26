#' @title Return the dimensions of an edsurvey.data.frame.
#' 
#' @description Return the dimensions of an \code{edsurvey.data.frame}.
#'
#' @param x an \code{edsurvey.data.frame}.
#' @param ... not used, kept for compatibility
#'
#' @return returns a numeric vector of length 2, with the first element
#'         being the number  of rows and the second element being the number
#'         of columns.
#' 
#' @author Paul Bailey
#' @export
dim.edsurvey.data.frame <- function(x, ...) {
  suppressWarnings(data <- getData(x,
                                   varnames = c(names(x$data)[1]),
                                   drop=TRUE,
                                   omittedLevels=FALSE,
                                   defaultConditions=FALSE))
  nrow <- length(data)
  ncol <- length(names.edsurvey.data.frame(x))
  return(c(nrow, ncol))
}

# this method used internally to get the number of rows on the original data
nrow2.edsurvey.data.frame <- function(x) {
  getAttributes(x, "dim0")[1]
}
