#' @title Instructions for Downloading and Unzipping ICILS Files
#'
#' @description Provides instructions to download ICILS data to be processed in \code{readICILS}.
#' 
#' @param year an integer vector indicating the study year. Valid year is 2013 only.
#' @author Tom Fink
#' 
#' @seealso \code{\link{readICILS}}
#' @export
downloadICILS <- function(year=c(2013)) {
  if(is.null(year)){
    stop(paste0("The argument ", sQuote("year"), " must not be null."))
  }
  
  if(any(!(year %in% c(2013)))){
    stop(paste0("The argument ", sQuote("year"), " must have a value of ", sQuote("2013"), "."))
  }
  
  linkURL <- "http://rms.iea-dpc.org/"
  
  txt <- c()
  txt <- c(txt, paste0("Please manually download and extract the SPSS (*.sav) formatted ICILS 2013 study data files from the IEA Data Repository to a folder on your local system or network. ",
                       "After the following steps are completed, the ", dQuote("readICILS"), " function could be used to read in the data. ",
                       "See help page (?readICILS) for more details."))
  txt <- c(txt, "\n")
  
  txt <- c(txt, paste0("\t", "1) Launch the IEA Data Repository web URL (", linkURL ,") in your web browser."))
  
  txt <- c(txt, paste0("\t", "2) Click on ", dQuote("Search"), " to view available studies. Under ", dQuote("Select Study"), ", use the study navigation tool to select the ", 
                       dQuote("ICILS"), " Study, Grade Level, and Study Year."))
  
  txt <- c(txt, paste0("\t", "3) Under ", dQuote("Select File Types and Countries"), ", select all file types (columns) available for the study, as well as all education systems (rows) you wish to analyze. ",
                       "The EdSurvey package will require all the available file types to be present for the read-in functions to work correctly. ",
                       "A specific country might be missing particular file types, which is normal."))
  
  txt <- c(txt, paste0("\t", "4) Next, under ", dQuote("Select Format"), ", select the ", dQuote("SPSS"), " option for the data file format. ",
                       "The EdSurvey package, requires SPSS (*.sav) formatted file types for the ", dQuote("readICILS"), " function."))
  
  txt <- c(txt, paste0("\t", "5) Optionally, under ", dQuote("Select Documentation"), ", you may select any codebooks, user guides, or additional documents relevant to the study."))
  
  txt <- c(txt, paste0("\t", "6) Next to ", dQuote("Download Name"), ", enter a filename into the textbox and click the ", dQuote("Add to Basket"), " button to create a zip file (*.zip) download specification with your selections."))
  
  txt <- c(txt, paste0("\t", "7) Click the ", dQuote("View Basket"), " button, or ", dQuote("Basket"), " tab, to see your newly created file. ",
                       "Download the zip container to a specified folder by clicking the ", sQuote("disk"), " icon and following your specific web browser prompts."))
  
  txt <- c(txt, paste0("\t", "8) Locate your downloaded zip file (*.zip) container and use an extraction program to extract the folder's file contents. ",
                       "It is recommended to extract the SPSS (*.sav) files to an easy-to-remember folder path based on the study and year (e.g., for Microsoft Windows OS, ", sQuote("C:/ICILS2013/"), ")."))
  
  txt <- c(txt, "\n")
  
  cat(paste(strwrap(paste(txt, collapse = "\n\n")),collapse="\n"),"\n\n")
  
  nav <- readline(prompt = paste0("Please enter 'Y' if you wish to launch this URL (", linkURL ,") in your browser:  "))
  
  if(tolower(trimws(nav))=="y"){
    browseURL(linkURL)
  }
}
