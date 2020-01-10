# utilities functions

getAllTaylorVars <- function(data) {
  res <- c(getAttributes(data, "psuVar"), getAttributes(data, "stratumVar"))
  res <- res[!res == ""]
  wgts <- getAttributes(data, "weights")
  for(wi in wgts) {
    if(!is.null(wi$stratumVar)) {
      res <- c(res, wi$stratumVar)
    }
    if(!is.null(wi$psuVar)) {
      res <- c(res, wi$psuVar)
    }
  }
  return(unique(res))
}

# This function returns default PSU variable name or a PSU variable associated with a weight variable
#' @rdname edsurvey-class
#' @export
getPSUVar <- function(data, weightVar=NULL) {
  if (is.null(weightVar)) {
    return(getAttributes(data,"psuVar"))
  }
  psuVar <- getAttributes(data,"psuVar")
  if (!is.null(psuVar) && psuVar != "") {
    return(psuVar)
  }
  weights <- getWeightByName(data, weightVar)
  return(weights$psuVar)
}

# This function returns default stratum variable name or a PSU variable associated with a weight variable
#' @rdname edsurvey-class
#' @export
getStratumVar <- function(data, weightVar=NULL) {
  if (is.null(weightVar)) {
    return(getAttributes(data,"stratumVar"))
  }
  stratumVar <- getAttributes(data,"stratumVar")
  if (!is.null(stratumVar) && stratumVar != "") {
    return(stratumVar)
  }
  weights <- getWeightByName(data, weightVar)
  return(weights$stratumVar)
}

# This function returns an attribute list of a given weight that includes:
# 1. jkbase
# 2. jksuffix
# 3. PSU variable (optional)
# 4. Stratum variable (optional)
getWeightByName <- function(data, weightVar) {
  weights <- getAttributes(data,"weights")
  weights <- weights[which(names(weights) == weightVar)]
  
  # Validate weightVar
  if (length(weights) == 0) {
    stop("The data does not have any weight called ", sQuote(weightVar),".")
  }
  if (length(weights) > 1) {
    warning(paste0("The data has more than one weight variable called ", sQuote(weightVar),". Using the first such weight."))
  }
  weights <- weights[[1]]
  return(weights)
}

# turns a vector into a properyly formatted list for showing the user
# @param vector a vector of items to paste
# @param final the word to put after the serial comma and the final element
# @
# examples:
# pasteItems(c())
# # NULL
# pasteItems(c("A"))
# # [1] "A"
# pasteItems(c("A", "B"))
# # [1] "A and B"
# pasteItems(c("A", "B", "C"))
# # [1] "A, B, and C"
# @author Paul Bailey
pasteItems <- function(vector, final="and") {
  # no need to do anything if there is one or fewer elements
  if(length(vector) <= 1) {
    return(vector)
  }
  if(length(vector) == 2) {
    return(paste0(vector[1], " ", final, " ", vector[2]))
  }
  v <- vector[-length(vector)]
  f <- vector[length(vector)]
  return(paste0(paste(v, collapse=", "), ", ", final, " ", f))
}

eout <- function(text, exdent=2, indent=0) {
  txto <- strwrap(text,
                  width=getOption("width")*0.9,
                  indent=indent,
                  exdent=exdent)
  writeLines(txto)
}

fixPath <- function(path) {
  dir <- dirname(path)
  base <- basename(path)
  flist <- list.files(dir)
  if(!dir.exists(dir)) {
    stop(paste0("Could not find directory ", dQuote(dir), "."))
  }
  if(base %in% flist) {
    # this is a good file path, return it
    return(file.path(dir,base))
  }
  if(tolower(base) %in% tolower(flist)) {
    return(file.path(dir, flist[which(tolower(flist) %in% tolower(base))]))
  }
  stop(paste0("Could not find file ", dQuote(file.path(dir, base)), "."))
}
