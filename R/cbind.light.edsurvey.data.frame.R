
#' @title Combine a data frame to an \code{light.edsurvey.data.frame}.
#'
#' @description Take a vector or \code{data.frame} and combine by columns to an \code{light.edsurvey.data.frame}.
#'
#' @param x a \code{light.edsurvey.data.frame}. The attributes of the resulting \code{light.edsurvey.data.frame} are taken from \code{x}.
#' @param ... one or more \code{light.edsurvey.data.frame}s, or \code{data.frame}s. 
#' @return a \code{light.edsurvey.data.frame} with the same attributes as the first argument, \code{x}.
#'
#' @seealso \code{\link[base]{cbind}} 
#' @method cbind light.edsurvey.data.frame
#' @author Michael Lee and Paul Bailey
#'
#' @method cbind light.edsurvey.data.frame
#' @export
cbind.light.edsurvey.data.frame <- function(x, ...) {
  # use the cbind method as if it were a data.frame
  res <- NextMethod("cbind") # res is the (preliminary) result
  # copy over all of the attributes
  # first, get their names
  atrs <- names(attributes(x))
  # remove data.frame attributes not in a light.edsurvey.data.frame
  atrs <- atrs[!atrs %in% c("names", "row.names", "class")]
  # then loop over all attributes and add them to the result
  lapply(atrs, function(z) {
    # make a temporary copy of res
    dat <- get("res")
    # add the attribute to the temporary copy
    attr(dat, z) <- attr(get("x"), z)
    # then make res (in the environment of the function) be the temporary
    # copy that now has this attribute
    res <<- dat 
  })
  if (inherits(res, "data.frame")) {
    class(res) <- class(x)
  } # End of test of data.frame inheritance
  res
}
