#' @title Subset the rows of a \code{light.edsurvey.data.frame}.
#'
#' @description Return a new \code{light.edsurvey.data.frame} which, when passed to \code{getData}, returns only a subset of the data.
#'
#' @param x a \code{light.edsurvey.data.frame}.
#' @param i not used. Included only for compatability.
#' @param ... not used. Included only for compatability.
#' @return An object of the same class as \code{x}, with the same attributes, that has been subsetted.
#' @author Paul Bailey
#' @examples
#' # read in the example data (generated, not real student data)
#' sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
#' gddat <- getData(sdf, c("composite", "dsex"), addAttributes = TRUE)
#' table(gddat[gddat$dsex=="Male","dsex"])
#' 
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
