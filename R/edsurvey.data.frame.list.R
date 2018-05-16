#' @title EdSurvey Dataset Vectorization
#'
#' @description \code{edsurvey.data.frame.list} creates an
#'              \code{edsurvey.data.frame.list} from a series of
#'              \code{edsurvey.data.frame} objects.
#'              \code{append.edsurvey.data.frame.list} creates an
#'              \code{edsurvey.data.frame.list} from two
#'              \code{edsurvey.data.frame} or \code{edsurvey.data.frame.list} objects.
#'
#'              An \code{edsurvey.data.frame.list} is useful for looking at
#'              data, for example, across time or graphically, and reduces
#'              repetition in function calls.
#'              The user may specify a variable that varies across the
#'              \code{edsurvey.data.frame} objects that is 
#'              then included in further output.
#'
#' @param datalist   a list of \code{edsurvey.data.frame}s to be combined
#' @param cov        a character vector that indicates what varies across
#'                   the \code{edsurvey.data.frame} objects.
#'                   See Examples. Guessed if not supplied. For example,
#'                   if several \code{edsurvey.data.frame}s for several
#'                   different countries are supplied, then \code{cov} would
#'                   be set to the country.
#' @param labels     a character vector that specifies labels. Must be the
#'                   same length
#'                   as \code{datalist}. Not needed if \code{cov} exists or can be
#'                   guessed. See Examples.
#' @param sdfA       an \code{edsurvey.data.frame} or \code{edsurvey.data.frame.list} to be combined
#' @param sdfB       an \code{edsurvey.data.frame} or \code{edsurvey.data.frame.list} to be combined
#' @param labelsA    a character vector that specifies \code{labels} for \code{sdfA} when creating 
#'                   the new \code{edsurvey.data.frame.list}. \code{labelsA} would be ignored
#'                   if \code{sdfA} is an \code{edsurvey.data.frame.list} with labels supplied.
#' @param labelsB    a character vector that specifies \code{labels} for \code{sdfB} when creating 
#'                   the new \code{edsurvey.data.frame.list}. \code{labelsB} would be ignored
#'                   if \code{sdfB} is an \code{edsurvey.data.frame.list} with labels supplied.
#'
#' @details 
#' The \code{edsurvey.data.frame.list} can be used in place of an
#' \code{edsurvey.data.frame} in function calls, and results are returned
#' for each of the component \code{edsurvey.data.frame}s, with the
#' organization of the results varying by the particular method.
#'
#' An \code{edsurvey.data.frame.list} can be created from several
#' \code{edsurvey.data.frame} objects that are related;
#' for example, all are NAEP mathematics assessments but have one or more
#' differences (e.g.,  they are all from different years). 
#' Another example could be data from multiple countries for an
#' international assessment.
#' 
#' When \code{cov} and \code{labels} are both missing, \code{edsurvey.data.frame.list}
#' attempts to guess what variables may be varying and uses those. When there are no
#' varying covariates, generic labels are automatically generated.
#'
#' @return
#' \code{edsurvey.data.frame.list} returns an \code{edsurvey.data.frame.list} with
#' elements
#' \item{\code{datalist}}{a list of \code{edsurvey.data.frame} objects}
#' \item{\code{covs}}{a character vector of key variables that vary within
#'                    the \code{edsurvey.data.frame.list}.
#'                    When labels are included, they will be included in
#'                    \code{covs}. In the unusual circumstance that \code{sdfA} or \code{sdfB}
#'                    is an \code{edsurvey.data.frame.list},
#'                    has \code{covs}, and labels are not supplied, the \code{covs}
#'                    are simply pasted together with colons between them.}
#'
#' \code{append.edsurvey.data.frame.list} returns an \code{edsurvey.data.frame.list} with
#' elements
#' \item{\code{datalist}}{a list of \code{edsurvey.data.frame} objects}
#' \item{\code{covs}}{a character vector of key variables that vary within
#'                    the \code{edsurvey.data.frame.list}.
#'                    When labels are included, they will be included in
#'                    \code{covs}.}
#' 
#' @author Paul Bailey and Huade Huo
#'
#' @example man/examples/edsurvey.data.frame.list.R 
#' @aliases append.edsurvey.data.frame.list 
#' @export
edsurvey.data.frame.list <- function(datalist, cov=NULL, labels=NULL) {
  # Search for covariates if no labels or covariates are provided 
  searching <- ifelse(is.null(cov) & is.null(labels), TRUE, FALSE)
  if(searching) {
    # these are the attributes that might vary
    cov <- c("subject", "year", "assessmentCode",
      "dataType", "gradeLevel", "survey", "achievementLevels",
      "country")
  }
  # the eventual covs result
  covs <- NULL
  # if the user did not provide the cov (no "s") matrix
  if(!is.null(cov)) {
    # for each column of cov (c)
    covs <- sapply(cov, function(c) {
              # for each edsurvey.data.frame (z)
              sapply(datalist, function(z) {
                # grab attribute c from edsurvey.data.frame z
                thisAttr <- getAttributes(z, c)
                # if this attribute is a character
                if(inherits(thisAttr, "character")) {
                  # if the attribute is a vector with more than one element we need to reduce it to a single element.
                  if(length(thisAttr) > 1) {
                    thisAttr <- paste(thisAttr, collapse="; ")
                  }
                } else {
                  thisAttr <- "" #return a blank character value in case of a missing cov value otherwise its returned as a list and won't load into the data.frame
                }
                thisAttr
              }, simplify=TRUE)
            }, simplify=FALSE)
    covs <- data.frame(covs)
  }

  if(searching) {
    for(i in ncol(covs):1){
      if(length(unique(covs[,i])) == 1) {
        if(i==1 & ncol(covs)==1) {
          # we have removed all of the columns.
          # So, the user asked us to automatically identify attributes that varried and we could not.
          # warn them ane just use LETTERS as labels
          warning("Cannot identify attributes that vary across elements in datalist. Using generated labels instead.")
          let <- LETTERS
          while(length(let) < nrow(covs)) {
            let <- paste0(rep(let,each=26),rep(LETTERS, length(let)))
          }
          covs$labels <- let[1:nrow(covs)]
        }
        # if there is no variation in the column, get rid of it
        covs <- covs[,-i, drop=FALSE]
      }
    }
  }
  
  if(!is.null(labels)) {
    if(length(labels) != length(datalist)) {
      stop(paste0("Length of argument ", sQuote("labels")," must be the same as the length of the ", sQuote("datalist"), " argument."))
    }
    if(is.null(covs)) {
      covs <- data.frame(labels=labels)
    } else {
      covs$labels <- labels
    }
  }
  # final results
  res <- list(datalist=datalist, covs=covs)
  class(res) <- "edsurvey.data.frame.list"
  return(res)
}

# @author Huade Huo and Paul Bailey
#
#' @rdname edsurvey.data.frame.list
#' @export
append.edsurvey.data.frame.list <- function(sdfA, sdfB, labelsA=NULL, labelsB=NULL) {
  # return a list of sdfs from either an edsurvey.data.frame.list or a single edsurvey.data.frame
  #TODO: I dont think this would be hugely hard to generalize to take more than two dfs or lists of dfs
  # perhaps using Reduce. Is that worth impelmenting? 
  getDataList <- function(sdf) {
    if (inherits(sdf, c("edsurvey.data.frame.list"))) {
      return(unlist(lapply(sdf[[1]], list), recursive = FALSE))
    } else {
      return(list(sdf))
    }
  }
  
  getDataLabel <- function(sdf, label) {
    if (inherits(sdf, c("edsurvey.data.frame.list"))) {
      if ("labels" %in% colnames(sdf[[2]])) {
        if (!is.null(label)) {
          warning(paste0("Ignored label since ", deparse(substitute(sdf)), " is an edsurvey.data.frame.list"))
          return(as.character(sdf[[2]]$labels))
        } else {
          return(as.character(sdf[[2]]$labels))
        }
      } else { # no labels on sdf[[2]]
        if (!is.null(label)) {
          return(label) # if there are labels, return them
        } else {
          # otherwise, slap labels together from existing covs
          return(apply(sdf[[2]],1, function(x) { paste0(as.character(x), collapse=":")}))
        }
      }
    } else {
      if(is.null(label)) {
        return("unlabeled element")
      }
      return(label)
    }
  }

  # bring it all together.
  return(edsurvey.data.frame.list(c(getDataList(sdfA), 
                                    getDataList(sdfB)), 
                                  labels = c(getDataLabel(sdfA, labelsA), 
                                             getDataLabel(sdfB, labelsB))))
}