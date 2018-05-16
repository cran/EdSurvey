#' @title Modify Variable Names
#'
#' @description Renames variables in an \code{edsurvey.data.frame},
#' a \code{light.edsurvey.data.frame} or an \code{edsurvey.data.frame.list}.
#' This function often is used when users want to conduct a gap analysis across
#'  years but variable names differ across two years of data. 
#'
#' @param x an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'          or an \code{edsurvey.data.frame.list}
#' @param oldnames a character vector of old variable names
#' @param newnames a character vector of new variable names to replace the
#'                 corresponding old names.
#' @param avoid_duplicated a logical value to indicate whether to avoid renaming the
#'                         variable if the corresponding new name already exists in the data. 
#'                         Defaults to \code{TRUE}.
#'
#' @details Note that all variable names are coerced to lowercase to comply with
#'          the \code{EdSurvey} standard.
#'
#' @return an object of the same class as \code{x} with new variable names
#' @export
#' 
#' @seealso \code{\link{gap}}
#' @author Trang Nguyen
#' @example \man\examples\rename.sdf.R
rename.sdf <- function(x,
                   oldnames,
                   newnames,
                   avoid_duplicated = TRUE){
  # Preparing/ checking arguments
  checkDataClass(x, c("edsurvey.data.frame.list","edsurvey.data.frame","light.edsurvey.data.frame"))
  oldnames <- tolower(unlist(oldnames))
  newnames <- tolower(unlist(newnames))
  if(length(oldnames) != length(newnames)){
    stop("Length of old variable names is not equal to length of new variable names. ")
  }
  
  if(inherits(x,"edsurvey.data.frame") || inherits(x,"light.edsurvey.data.frame")) {
    # list of elements that is involved in the rename process
    userConditions <- getAttributes(x,"userConditions")
    pvvars <- getAttributes(x,"pvvars")
    weights <- getAttributes(x, "weights")
    if (inherits(x,"edsurvey.data.frame")) {
      varnames <-  names(x$data)
    } else {
      varnames <- colnames(x)
    }
    fileFormat <- getAttributes(x,"fileFormat")
    fileFormatSchool <- getAttributes(x, "fileFormatSchool")
    fileFormatTeacher <- getAttributes(x,"fileFormatTeacher")
    dataListMeta <- getAttributes(x,"dataListMeta")
    
    for (vari in 1:length(oldnames)) {
      # to avoid duplicates after the operation
      if (newnames[vari] %in% c(varnames, names(weights), names(pvvars))) {
        if (avoid_duplicated) {
          warning("Variable name ",sQuote(newnames[vari]), " already exists in the data. Not renaming the variable to avoid duplicates. ")
          next
        } else {
          warning("Variable name ",sQuote(newnames[vari]), " already exists in the data. Renaming the variable to ",sQuote(paste0(newnames[vari],"_2")),". ")
          newnames[vari] <- paste0(newnames[vari],"_2")
        }
      }
      ## change the name in fileformat
      varn <- toupper(oldnames[vari])
      if(varn %in% fileFormat$variableName) {
        fileFormat$variableName[fileFormat$variableName == toupper(oldnames[vari])] <- toupper(newnames[vari])
      }
      
      if(varn %in% fileFormatSchool$variableName) {
        fileFormatSchool$variableName[fileFormatSchool$variableName == toupper(oldnames[vari])] <- toupper(newnames[vari])
      }
      
      if(varn %in% fileFormatTeacher$variableName) {
        fileFormatTeacher$variableName[fileFormatTeacher$variableName == toupper(oldnames[vari])] <- toupper(newnames[vari])
      }
      # change variable name in userConditions list
      if (!is.null(userConditions) && length(userConditions) > 0) {
        for (i in 1:length(userConditions)) {
          if (!is.null(names(userConditions)[i]) && names(userConditions)[i] %in% "recode") {
            names(userConditions[[i]]) <- gsub(paste0("\\b",oldnames[vari],"\\b"),newnames[vari], names(userConditions[[i]]))
          } else {
            condition <- userConditions[[i]]
            userConditions[[i]] <- replaceVars(condition, oldnames[vari], newnames[vari])
          }
        } # end (for(i in 1:length(userConditions)))
      } # end if (!is.null(userConditons))
     
      # change pvvars
      if(!is.null(pvvars)) {
        if (oldnames[vari] %in% names(pvvars)) {
          names(pvvars)[names(pvvars) == oldnames[vari]] <- newnames[vari]
          attr(pvvars,'default') <- gsub(paste0("\\b", oldnames[vari],"\\b"), newnames[vari], attr(pvvars,'default'))
          next
        }
      }
      
      # change weights
      if (oldnames[vari] %in% names(weights)) {
        names(weights)[names(weights) == oldnames[vari]] <- newnames[vari]
        attr(weights,"default") <- gsub(paste0("\\b",oldnames[vari],"\\b"), newnames[vari], attr(weights,"default"))
        if (oldnames[vari] %in% varnames) {
          varnames[varnames == oldnames[vari]] <- newnames[vari]
        }
        next
      }
      
      if(!oldnames[vari] %in% varnames) {
        warning(paste0(oldnames[vari]," is not in the data.\n"))
        next
      }
      
      ## change the names in names(x)
      varnames[varnames == oldnames[vari]] <- newnames[vari]
      
      # if the name is one of the plausible values
      if(!is.null(pvvars)) {
        for (pvi in 1:length(pvvars)) {
          pvvars[[pvi]]$varnames <- gsub(paste0("\\b",oldnames[vari],"\\b"),newnames[vari], pvvars[[pvi]]$varnames)
        }  
      }
      
      ## change link id
      if(!is.null(dataListMeta)) {
        dataListMeta$student$school <- gsub(paste0("\\b",oldnames[vari],"\\b"),newnames[vari],dataListMeta$student$school)
        dataListMeta$student$teacher <- gsub(paste0("\\b",oldnames[vari],"\\b"),newnames[vari],dataListMeta$student$teacher)
      }
    } # end (for(vari in 1:length(oldnames)))
    
    # replace all of attributes
    if (inherits(x, "light.edsurvey.data.frame")) {
      names(x) <- varnames
    } else {
      names(x$data) <- varnames
    }
    
    if (!is.null(userConditions)) { x <- setAttributes(x,"userConditions", userConditions)}
    if (!is.null(pvvars)) { x <- setAttributes(x,"pvvars", pvvars) }
    if (!is.null(weights)) { x <- setAttributes(x, "weights", weights) }
    if (!is.null(fileFormat)) { x <- setAttributes(x,"fileFormat", fileFormat) }
    if (!is.null(fileFormatSchool)) { x <- setAttributes(x, "fileFormatSchool", fileFormatSchool) }
    if (!is.null(fileFormatTeacher)) { x <- setAttributes(x,"fileFormatTeacher", fileFormatTeacher) }
    if (!is.null(dataListMeta)) { x <- setAttributes(x,"dataListMeta",dataListMeta) }
    
    return(x)
  } else if (inherits(x,"edsurvey.data.frame.list")) {
    # assuming that variable names are consistent through the data list
    for (i in 1:length(x$datalist)) {
      x$data[[i]] <- rename.sdf(x$data[[i]], oldnames, newnames)
      x$datalist[[i]] <- x$data[[i]]
    }
    return(x)
  }
}

replaceVars <- function(condition, oldname, newname) {
  for (ci in 1:length(condition)) {
    if (class(condition[[ci]]) %in% "name" && length(condition[[ci]]) == 1 && as.character(condition[[ci]]) %in% oldname) {
      condition[[ci]] <- as.name(newname)
    }
    if (class(condition[[ci]]) %in% "call" || length(condition[[ci]]) > 1) {
      condition[[ci]] <- replaceVars(condition[[ci]], oldname, newname)
    }
  } # end for(ci in 1:length(conditon))
  condition
}