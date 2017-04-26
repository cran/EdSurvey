#' @title Return the names of an edsurvey.data.frame.
#' 
#' @description Return the column names of an \code{edsurvey.data.frame}.
#'
#' @param x an \code{edsurvey.data.frame}.
#' @param ... not used. Included only for compatability.
#' @return A vector showing the names of variables in an \code{edsurvey.data.frame}.
#' 
#' @author Michael Lee and Paul Bailey
#' @export
names.edsurvey.data.frame <- function(x, ...) {
  return(c(names(x$data), names(x$dataSch)))
}
