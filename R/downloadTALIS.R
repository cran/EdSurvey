#' @title Instructions for Downloading TALIS Files
#'
#' @description Provides instructions to download TALIS data to be processed in \code{\link{readTALIS}}. 
#' 
#' @param year a numeric value indicating the assessment year to download. Available years are 2008 and 2013.
#' @author Trang Nguyen
#' @importFrom utils browseURL
#' @export
downloadTALIS <- function(year) {
  if (!year %in% c(2008,2013)) {
       stop(sQuote(year), "is not a valid year. TALIS data is available for 2008 and 2013 only.")
  }
  url <- "http://stats.oecd.org/Index.aspx?datasetcode=talis_2013%20"
  cat("Currently there is no way to download TALIS data automatically from the webpage. Please use the link below and follow the instruction \n to retrieve the relevant data files.")
  cat("\n \t1) Click the following link: http://stats.oecd.org/Index.aspx?datasetcode=talis_2013%20")
  if (year == 2008) {
    cat("\n \t2) Go to 2008 Complete Database section and select 'Related files' from the dropdown menu 'Export'. Download the combined zip file (SPSS combined.zip) to the directory '/TALIS/2008'")
  } else if (year == 2013) {
    cat("\n \t2) Go to 2013 Complete Database section and select 'Related files' from the dropdown menu 'Export'. Download the combined zip file (SPSS International.zip) to the directory '/TALIS/2013'")
  }
  cat("\n")
  nav <- readline(prompt = "Please enter 'Y' if you wish to launch this URL in your browser: ")
  if (tolower(trimws(nav)) == "y") {
    browseURL("http://stats.oecd.org/Index.aspx?datasetcode=talis_2013%20")
  }
}