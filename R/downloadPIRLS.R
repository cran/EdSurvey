#' @title Download and Unzip PIRLS Files
#'
#' @description Uses an Internet connection to download PIRLS data. 
#'              Data come from \href{https://timssandpirls.bc.edu/}{timssandpirls.bc.edu} zip files. This
#'              function works for 2001, 2006, and 2011 data.
#'
#' @param years an integer vector of the assessment years to download. Valid years are 2001, 2006, and 2011.
#'              
#' @param root a character string indicating the directory where the PIRLS
#'             data should be stored. Note that files are placed in a
#'             subdirectory named PIRLS[year].
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data.  Default value is \code{FALSE}.
#' @author Tom Fink
#' @seealso \code{\link{readPIRLS}}
#' @example man/examples/downloadPIRLS.R
#' @importFrom utils download.file
#' @export
downloadPIRLS <- function(years=c(2001, 2006, 2011), root, cache=FALSE) {
  if(is.null(root)){
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if(length(unlist(root))!=1){
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }
  
  validYears <- c(2001, 2006, 2011)
  if(length(years) > 1) {
    for(yi in years) {
      downloadPIRLS(years=yi, root=root, cache=cache)
    }
    return()
  } else {
    year <- years
  }
  
  d2001 <- c("https://timssandpirls.bc.edu/pirls2001i/Pirls2001Database/pirls_2001_spssdata.zip")
             
  d2006 <- c("https://timssandpirls.bc.edu/PDF/PIRLS2006_SPSSData.zip")
  
  d2011 <- c("https://timssandpirls.bc.edu/pirls2011/downloads/P11_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/pirls2011/downloads/P11_SPSSData_pt2.zip")
  
  if(!year %in% validYears) {
    stop(paste0("Only know years are ", paste(validYears, collapse=", "), "."))
  }
  yroot <- file.path(root, paste0("PIRLS",year)) #build yroot with file.path to avoid issues with seperators
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
      cat(paste0("Found downloaded ", year ," PIRLS file ",bn,"\n"))
    }
    lst <- unzip(bn, list=TRUE) # just lists the files
    cat(paste0("Unzipping ", year ," PIRLS files from ",bn,"\n"))
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
    cat("Caching ", year ," PIRLS files\n")
    
    notUsed <- readPIRLS(yroot, countries="*", verbose=TRUE)
    notUsed <- NULL #clear memory space
    
    setwd(prevWD)
    return()
  }
  
  setwd(prevWD)
}
