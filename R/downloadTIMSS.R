#' @title Download and Unzip TIMSS Files
#'
#' @description Uses an Internet connection to download TIMSS data.
#'              Data come from \href{https://timssandpirls.bc.edu/}{timssandpirls.bc.edu} zip files. This
#'              function works for 2003, 2007, 2011, and 2015 data.
#'
#' @param years an integer vector of the assessment years to download. Valid years are 2003, 2007, 2011,
#'              and 2015.
#' @param root a character string indicating the directory where the TIMSS
#'             data should be stored. Note that files are placed in a
#'             subdirectory named TIMSS[year].
#' @param cache a logical value set to process and cache the txt version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' @author Tom Fink
#' @seealso \code{\link{readTIMSS}}
#' @example man/examples/downloadTIMSS.R
#' @importFrom utils download.file
#' @export
downloadTIMSS <- function(years=c(2003, 2007, 2011, 2015), root, cache=FALSE) {
  if(is.null(root)){
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if(length(unlist(root))!=1){
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }
  
  validYears <- c(2003, 2007, 2011, 2015)
  if(length(years) > 1) {
    for(yi in years) {
      downloadTIMSS(years=yi, root=root, cache=cache)
    }
    return()
  } else {
    year <- years
  }

  d2003 <- c("https://timssandpirls.bc.edu/timss2003i/PDF/t03_spss_1.zip",
             "https://timssandpirls.bc.edu/timss2003i/PDF/t03_spss_2.zip")

  d2007 <- c("https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G4_1.zip",
             "https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G4_2.zip",
             "https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G8_1.zip",
             "https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G8_2.zip",
             "https://timssandpirls.bc.edu/TIMSS2007/PDF/T07_SPSS_G8_3.zip")

  d2011 <- c("https://timssandpirls.bc.edu/timss2011/downloads/T11_G4_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G4_SPSSData_pt2.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G4_SPSSData_pt3.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G8_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G8_SPSSData_pt2.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G8_SPSSData_pt3.zip",
             "https://timssandpirls.bc.edu/timss2011/downloads/T11_G8_SPSSData_pt4.zip")

  d2015 <- c("https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_SPSSData_pt2.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G4_SPSSData_pt3.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_SPSSData_pt1.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_SPSSData_pt2.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_SPSSData_pt3.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/T15_G8_SPSSData_pt4.zip",
             "https://timssandpirls.bc.edu/timss2015/international-database/downloads/TN15_SPSSData.zip")

  if(!year %in% validYears) {
    stop(paste0("Only know years are ", paste(validYears, collapse=", "), "."))
  }
  yroot <- file.path(root, paste0("TIMSS", year)) #build yroot with file.path to avoid issues with seperators
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  d <- get(paste0("d",year))
  for(di in 1:length(d)) {
    bn <- basename(d[di]) # name of the file (without the path)
    if(!file.exists(file.path(yroot,bn))) {
      # required for download
      options(HTTPUserAgent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0")
      # download zip and catch any errors
      tryCatch(download.file(d[di],file.path(yroot,bn)),
                   error = function(e){
                     stop(paste0("Error downloading file at URL: ", sQuote(d[di]),
                                 " Message: ", e))
                   })
    } else {
      cat(paste0("Found downloaded ", year ," TIMSS file ",bn,"\n"))
    }
    lst <- unzip(file.path(yroot,bn), list=TRUE) # just lists the files
    cat(paste0("Unzipping ", year ," TIMSS files from ",bn,"\n"))
    for(i in 1:nrow(lst)) {
      if(!file.exists(file.path(yroot,basename(lst$Name[i]))) | file.info(file.path(yroot,basename(lst$Name[i])))$size != lst$Length[i]) {
        cat(paste0("  unzipping ",lst$Name[i],"\n"))
        unzip(file.path(yroot,bn),files= lst$Name[i], exdir = yroot)
        if(basename(lst$Name[i]) != lst$Name[i]) {
          file.rename(file.path(yroot,lst$Name[i]), file.path(yroot,basename(lst$Name[i])))
        }
      }
    }
  }
  if(cache){
    cat("Caching ", year ," TIMSS files\n")
    notUsed <- readTIMSS(yroot, countries="*", gradeLvl=4, verbose=TRUE)
    notUsed <- NULL
    notUsed <- readTIMSS(yroot, countries="*", gradeLvl=8, verbose=TRUE)
    notUsed <- NULL
    return()
  }
}
