#' EdSurvey: a package to analyze NAEP data.
#' 
#' The EdSurvey package uses appropriate methods for analysis of the NCES
#' data sets with a small memory
#' footprint. Existing system control files, included with the
#' data, are used
#' to read in and format the data for further processing.
#' 
#' To get started using EdSurvey, see the vignettes covering
#' installation, first use, use of the
#' \code{getData} function to get data than can be further manipulated
#' or analyzed, and use of the accommodations datasets. Use 
#' \code{browseVignettes(package="EdSurvey")} to see the vignettes.
#' 
#' The package provides a function called \code{\link{readNAEP}}
#' to read in NAEP data.
#' The functions \code{\link{lm.sdf}}, \code{\link{cor.sdf}},
#' \code{\link{edsurveyTable}}, and \code{\link{achievementLevels}} 
#' can then be used to analyze data while \code{\link{getData}} extracts
#' the data of interest as a data.frame for further processing.
#' 
#' @docType package
#' @name EdSurvey-package
NULL