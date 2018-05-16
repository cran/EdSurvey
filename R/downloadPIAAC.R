#' @title Download and Unzip PIAAC Files
#'
#' @description Uses a connection to download PIAAC data to a
#'              computer. Data come from the OECD website. 
#' 
#' @param round a numeric value indicating the assessment round to download. Available round now is only 1 for 2012/2014 (the default).
#' @param root a character string indicating the directory where the PIAAC data should
#'             be stored. Note that files are placed in a folder named PIAAC and then in
#'             a year subdirectory.
#' @param cache a logical value set to \code{FALSE} to cache .txt versions of files. If set to \code{TRUE}, the function will
#'        process all downloaded files, which might take several hours. 
#' @param verbose a logical value that determines if you want verbose output while the function is running to indicate the progress.
#'        Defaults to \code{TRUE}.  
#' 
#' @importFrom stringr str_subset
#' @importFrom rvest html html_nodes html_attr
#' @import magrittr 
#' @author Trang Nguyen
#' @export
downloadPIAAC <- function(root, round=1, cache=FALSE, verbose=TRUE) {
  valid_rounds <- c(1)
  if (!round %in% valid_rounds) {
    stop("PIAAC is not available for this round.")
  }
  root = gsub("/$","", root)
  dirname = file.path(root,"PIAAC")
  if(!dir.exists(dirname)) {
    dir.create(dirname)
  }
  yroot <- file.path(dirname, paste0("Round ",round))
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  
  if(round == 1) {
    url = "http://vs-web-fs-1.oecd.org/piaac/puf-data/CSV/"
  }
  
  temp <- tryCatch({
    data_files <- xml2::read_html(url) %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset("\\.csv$")
  }, error = function(cond) {
    message("Error occurs when collecting downloadable file links. Error: ", cond)
    return(0)
  })
  if (length(temp) == 0) {
    cat("There is no file to be downloaded.\n")
    return(NULL)
  }
  
  codebook <- 'http://www.oecd.org/site/piaac/International%20Codebook_PIAAC%20Public-use%20File%20(PUF)%20Variables%20and%20Values.xlsx' 
  for (f in data_files) {
    fn <- basename(f)
    if(!file.exists(file.path(yroot,fn))) {
      download.file(paste0("http://vs-web-fs-1.oecd.org",f),file.path(yroot,fn), mode = "w", method = "auto")
    }
  }
  fn <- "international-codebook.xlsx"
  if(!file.exists(file.path(yroot,fn))) {
    download.file(codebook,file.path(yroot,fn), mode = "wb")
  }
  
  test <- tryCatch(readxl::read_excel(file.path(yroot,fn), sheet = 1),
                   error = function(cond) {
                     cache <<- FALSE
                     cat("The downloaded codebook file is corrupt. You need to manually download the codebook at the given link: ",sQuote(codebook),"to the folder ", sQuote(yroot),"\n")
                     nav <- readline(prompt = "Please enter 'Y' if you wish to launch this URL in your browser: ")
                     if (tolower(trimws(nav)) == "y") {
                       browseURL(codebook)
                     }
                   })
  if (cache) {
    notUsed <- readPIAAC(yroot, countries = "*", verbose = verbose)
    notUsed <- NULL
  }
}
