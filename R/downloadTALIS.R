#' @title Instructions for Downloading TALIS Files
#'
#' @description Provides instructions to download TALIS data to be processed in \code{\link{readTALIS}}. 
#' 
#' @param years a numeric value indicating the assessment year. Available years are 2008 and 2013.
#'
#' @author Trang Nguyen
#' @seealso \code{\link{readTALIS}}
#' @importFrom utils browseURL
#' @example  man/examples/downloadTALIS.R
#' @export
downloadTALIS <- function(years) {
  if (!years %in% c(2008,2013)) {
       stop(sQuote(years), "is not a valid years. TALIS data is available for 2008 and 2013 only.")
  }
  if (years == 2008) {
    url <- "http://stats.oecd.org/Index.aspx?datasetcode=talis"
    
  } else {
    url <- "http://stats.oecd.org/Index.aspx?datasetcode=talis_2013"
  }
  cat("Currently R cannot download TALIS data automatically from the website. Please use the link below and follow the instruction to retrieve the relevant data files.")
  cat(paste0("\n \t1) Click the following link: ", dQuote("http://stats.oecd.org/Index.aspx?datasetcode=talis_2013%20"),"."))
  if (years == 2008) {
    cat("\n \t2) Go to 2008 Complete Database section and select 'Related files' from the dropdown menu 'Export'. 
        Download the combined zip file (SPSS combined.zip) to the directory '/TALIS/2008'.")
  } else if (years == 2013) {
    cat("\n \t2) Go to 2013 Complete Database section and select 'Related files' from the dropdown menu 'Export'. 
        Download the combined zip file (SPSS International.zip) to the directory '/TALIS/2013'.")
  }
  cat("\n")
  nav <- readline(prompt = "Please enter 'Y' if you wish to launch this URL in your browser: ")
  if (tolower(trimws(nav)) == "y") {
    browseURL("http://stats.oecd.org/Index.aspx?datasetcode=talis_2013%20")
  }
}
