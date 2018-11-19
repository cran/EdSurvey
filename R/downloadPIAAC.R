#' @title Download and Unzip PIAAC Files
#'
#' @description Uses a connection to download PIAAC data to a
#'              computer. Data come from the OECD website. 
#' 
#' @param root a character string indicating the directory where the PIAAC data should
#'             be stored. Files are placed in a folder named PIAAC/Round [round number].
#' @param round a numeric value indicating the assessment round to download. Valid round is 1 only (2012/2014).
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#' 
#' @importFrom xml2 read_html
#' @importFrom readxl read_excel
#' @importFrom stringr str_subset
#' @importFrom rvest html html_nodes html_attr
#' @author Trang Nguyen
#' @example man/examples/downloadPIAAC.R
#' @export
downloadPIAAC <- function(root, round=1, cache=FALSE, verbose=TRUE) {
  valid_rounds <- c(1)
  if (!round %in% valid_rounds) {
    stop("PIAAC is not available for this round.")
  }
  root <- gsub("/$","", root)
  dirname <- file.path(root,"PIAAC")
  if(!dir.exists(dirname)) {
    dir.create(dirname)
  }
  yroot <- file.path(dirname, paste0("Round ",round))
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  
  if(round == 1) {
    url <- "http://vs-web-fs-1.oecd.org/piaac/puf-data/CSV/"
  }
  
  temp <- tryCatch({
    xml <- read_html(url)
    xml <- html_nodes(xml,"a")
    xml <- html_attr(xml, "href")
    data_files <- str_subset(xml, "\\.csv$")
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
    } else {
      if (verbose) {
        cat(paste0("Found downloaded round ",round, " PIAAC file ",fn, ".\n"))
      }
    }
  }
  fn <- "international-codebook.xlsx"
  if(!file.exists(file.path(yroot,fn))) {
    download.file(codebook,file.path(yroot,fn), mode = "wb")
  } else {
    if (verbose) {
      cat(paste0("Found downloaded round ",round," PIAAC codebook file ",fn, ".\n"))
    }
  }
  
  test <- tryCatch(read_excel(file.path(yroot,fn), sheet = 1),
                   error = function(cond) {
                     cache <<- FALSE
                     cat(paste0("The downloaded codebook file is corrupt. You need to manually download the codebook at the given link: ",sQuote(codebook)," to the folder ", sQuote(yroot),".\n"))
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
