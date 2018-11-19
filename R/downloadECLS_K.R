#' @title Download and Unzip ECLS_K Files
#'
#' @description Uses an Internet connection to download ECLS_K data. 
#'              Data come from \href{https://nces.ed.gov/edat/}{nces.ed.gov} zip files. This
#'              function works for 1998 and 2011 data.
#'
#' @param root a character string indicating the directory where the ECLS_K
#'             data should be stored. Files are placed in a
#'             subdirectory named ECLS_K/[year].
#' @param years an integer vector of the assessment years to download. Valid years are 1998 and 2011.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#'                
#' @author Tom Fink
#' @seealso \code{\link{readECLS_K1998}} and \code{\link{readECLS_K2011}}
#' @example man\examples\downloadECLS_K.R
#' @importFrom utils download.file
#' @export
downloadECLS_K <- function(root, years=c(1998, 2011), cache=FALSE, verbose=TRUE) {
  if(is.null(root)){
    stop(paste0("The argument ", sQuote("root"), " must be specified."))
  }
  if(length(unlist(root))!=1){
    stop(paste0("The argument ", sQuote("root"), " must be of length 1."))
  }
  
  #normalize path before testing:: file.path will remove trailing seperator if present
  root <- suppressWarnings(file.path(normalizePath(root, winslash = "/")))
  if(!dir.exists(root)){
    stop(paste0("The argument ", sQuote("root"), " must be a valid path."))
  }
  
  validYears <- c(1998, 2011)
  if(length(years) > 1) {
    for(yi in years) {
      downloadECLS_K(years=yi, root=root, cache=cache, verbose=verbose)
    }
    return(invisible(NULL))
  } else {
    year <- years
  }
  
  d1998 <- c("https://nces.ed.gov/edat/data/zip/ECLSK_1998-99_v1_0_ASCII_Datasets.zip",
             "https://nces.ed.gov/edat/data/zip/ECLSK_1998-99_v1_0_CodeBook_Layout.zip")
  
  d2011 <- d <- c("https://nces.ed.gov/ecls/data/ChildK4p.zip",
                  "https://nces.ed.gov/ecls/data/ECLSK2011_K4PUF.sps")
  
  if(!year %in% validYears) {
    stop(paste0("Only known years are ", pasteItems(validYears), "."))
  }
  
  #check if base folder exists first
  if(!dir.exists(file.path(root, "ECLS_K"))){
    dir.create(file.path(root, "ECLS_K"))
  }
  yroot <- file.path(root, "ECLS_K", paste0(year)) #build yroot with file.path to avoid issues with seperators
  if(!dir.exists(yroot)) {
    dir.create(yroot)
  }
  
  d <- get(paste0("d",year))
  
  #loop through each defined .zip file defined for the year
  for(di in 1:length(d)) {
    bn <- basename(d[di]) # name of the file (without the path)
    if(!file.exists(file.path(yroot,bn))) {
      # download
      tryCatch(download.file(d[di],file.path(yroot,bn), quiet = !verbose, cacheOK = FALSE),
               error = function(e){
                 stop(paste0("Error downloading file at URL: ", sQuote(d[di]),
                             " Message: ", e))
               })
    } else {
      if(verbose){
        cat(paste0("Found downloaded ", year ," ECLS_K file ",bn,"\n"))
      }
    }
    
    if(grepl("\\.zip$", bn, ignore.case = TRUE)){
  
      lst <- unzip(file.path(yroot,bn), list=TRUE) # just lists the files
      
      if(verbose){
        cat(paste0("Unzipping ", year ," ECLS_K files from ",bn,"\n"))
      }
      
      for(i in 1:nrow(lst)) {
        #check that the file is not present in root folder OR that the file sizes are different indicating a file change/corruption
        if(!file.exists(file.path(yroot, basename(lst$Name[i]))) | file.info(file.path(yroot, basename(lst$Name[i])))$size != lst$Length[i]) {
          if(verbose) {
            cat(paste0("  unzipping ",lst$Name[i],"\n"))
          }
          unzip(file.path(yroot,bn), files=lst$Name[i], exdir = yroot)
          if(basename(lst$Name[i]) != lst$Name[i]) {
            file.rename(file.path(yroot,lst$Name[i]), file.path(yroot,basename(lst$Name[i])))
          }
        }
      }
    }#end if(grepl("\\.zip$", bn, ignore.case = TRUE))
  }#end for(di in 1:length(d))
  
  if(cache) {
    if(verbose){
      cat("Caching ", year ," ECLS_K files\n")
    }
    
    if(year==2011) {
      notUsed <- readECLS_K2011(path = yroot, filename = "childK4p.dat", layoutFilename = "ECLSK2011_K4PUF.sps", verbose = verbose)
    }
  }
  
  return(invisible(NULL))
}