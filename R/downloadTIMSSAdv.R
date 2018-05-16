#' @title Download and Unzip TIMSS Advanced Files
#'
#' @description Uses an Internet connection to download TIMSS Advanced data.
#'              Data comes from \href{https://timssandpirls.bc.edu/}{timssandpirls.bc.edu} zip files. This
#'              function works for 1995, 2008, and 2015 data.
#'
#' @param years an integer vector of the assessment years to download. Valid years are 1995, 2008, and 2015.
#'              
#' @param root a character string indicating the directory where the TIMSS Advanced
#'             data should be stored. Note that files are placed in a
#'             subdirectory named TIMSSAdv[year].
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' 
#' @author  Tom Fink
#' @seealso \code{\link{readTIMSSAdv}}
#' @example man/examples/downloadTIMSSAdv.R
#' @importFrom utils download.file
#' @export
downloadTIMSSAdv <- function(years=c(1995, 2008, 2015), root, cache=FALSE) {
  if(is.null(root)){
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if(length(unlist(root))!=1){
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }
  
  validYears <- c(1995, 2008, 2015)
  if(length(years) > 1) {
    for(yi in years) {
      downloadTIMSSAdv(years=yi, root=root, cache=cache)
    }
    return()
  } else {
    year <- years
  }
  
  d1995 <- c("https://timssandpirls.bc.edu/timss_advanced/downloads/TA95_SPSS_Data.zip")
  
  d2008 <- c("https://timssandpirls.bc.edu/timss_advanced/downloads/TA08_SPSS_Data.zip")
  
  d2015 <- c("https://timssandpirls.bc.edu/timss2015/advanced-international-database/downloads/TA15_SPSSData.zip")
  
  if(!year %in% validYears) {
    stop(paste0("Only know years are ", paste(validYears, collapse=", "), "."))
  }
  yroot <- file.path(root, paste0("TIMSSAdv",year)) #build yroot with file.path to avoid issues with seperators
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  
  prevWD <- getwd()
  setwd(yroot)
  
  d <- get(paste0("d",year))
  for(di in 1:length(d)) {
    bn <- basename(d[di]) # name of the file (without the path)
    if(!file.exists(bn)) {
      # required for download
      options(HTTPUserAgent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0")
      # download
      tryCatch(download.file(d[di],bn),
               error = function(e){
                 stop(paste0("Error downloading file at URL: ", sQuote(d[di]),
                             " Message: ", e))
               })
    } else {
      cat(paste0("Found downloaded ", year ," TIMSS Advanced file ",bn,"\n"))
    }
    lst <- unzip(bn, list=TRUE) # just lists the files
    cat(paste0("Unzipping ", year ," TIMSS Advanced files from ",bn,"\n"))
    for(i in 1:nrow(lst)) {
      if(!file.exists(basename(lst$Name[i])) | file.info(basename(lst$Name[i]))$size != lst$Length[i]) {
        cat(paste0("  unzipping ",lst$Name[i],"\n"))
        unzip(bn,files=lst$Name[i])
        if(basename(lst$Name[i]) != lst$Name[i]) {
          file.rename(lst$Name[i], basename(lst$Name[i]))
        }
      }
    }
  }
  
  if(cache){
    cat("Caching ", year ," TIMSS Advanced files\n")
    
    notUsed <- readTIMSSAdv(yroot, countries="*", subject = "math", verbose=TRUE)
    notUsed <- NULL #clear memory space
    
    notUsed <- readTIMSSAdv(yroot, countries="*", subject = "physics", verbose=TRUE)
    notUsed <- NULL #clear memory space
    
    setwd(prevWD)
    return()
  }
  
  setwd(prevWD)
}

