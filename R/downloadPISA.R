#' @title Download and Unzips PISA Files
#'
#' @description Uses a connection to download PISA data to a
#'              computer. Data come from the OECD website. 
#' 
#' @param year the assessment years to download. Available years are 2000, 2003,
#'             2006, 2009, and 2012. To download data for all available years,
#'             users can input \code{*} (the default).
#' @param database a character to indicate which database to download from. For 2012,
#'              three databases are available (INT = International, CBA = Computer-Based Assessment, and
#'              FIN = Financial Literacy). Defaults to \code{INT}.
#' @param root a character string indicating the directory where the PISA data should
#'             be stored. Note that files are placed in a folder named PISA and then in
#'             a year subdirectory.
#' @param cache a logical value set to \code{FALSE} to cache .txt versions of files. If set to \code{TRUE}, the function will
#'        process all downloaded files, which might take several hours. 
#' @param verbose a logical value that determines if you want verbose output while the function is running to indicate the progress.
#'        Defaults to \code{TRUE}.   
#' @author Trang Nguyen
#' @importFrom utils unzip
#' @export
downloadPISA <- function(year="*", database="INT", root, cache=FALSE, verbose=TRUE) {
  # valid years for PISA
  validYears <- c(2000,2003,2006,2009,2012,2015)
  if ("*" %in% year) {
    year <- validYears
  }
  year <- as.numeric(year)
  for (y in year) {
    if(verbose) {
      cat(paste0("\n Processing PISA data for year ", y, "\n"))
    }
    if (y == 2015) {
      cat("\n PISA 2015 is currently on hold to wait for FWF file distribution license from OECD. \n")
      next
    }
    if (!y %in% validYears) {
      warning(sQuote(y), " is not a valid year. PISA had data for the following year: ", paste0(validYears, sep = " "))
      next
    }
    
    # Create a year root directory
    baseroot <- paste0(root,"PISA/")
    if(!dir.exists(baseroot)) {
      dir.create(baseroot)
    }
    yroot <- paste0(baseroot,y)
    if(!dir.exists(yroot)) {
      dir.create(yroot)
    }
    
    # Download all files
    collected_files <- pisaURLDat(y,database)
    for (f in collected_files) {
      fn <- basename(f)
      if(!file.exists(file.path(yroot,fn))) {
        #options(HTTPUserAgent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0")
        if(grepl("http",f, ignore.case = T)) {
          download.file(f,file.path(yroot,fn))
        } else {
          download.file(paste0("http://www.oecd.org/",f),file.path(yroot,fn))
        }
      }
    }
    # Unzipping files
    zFiles <- list.files(yroot,pattern = "\\.zip$", ignore.case = T, full.names = F)
    zFiles <- file.path(yroot,zFiles)
    for (z in zFiles) {
      lst <- unzip(z, list = TRUE)
      for(i in 1:nrow(lst)) {
        if(!file.exists(file.path(yroot,basename(lst$Name[i]))) | file.info(file.path(yroot,basename(lst$Name[i])))$size != lst$Length[i]) {
          unzip(z,files=lst$Name[i], exdir = yroot)
          if(basename(lst$Name[i]) != lst$Name[i]) {
            file.rename(file.path(yroot,lst$Name[i]), file.path(yroot,basename(lst$Name[i])))
          }
        }
      }
    }
    # Process files if required
    if (cache) {
      suppressWarnings(notUsed <- readPISA(yroot, database = database, countries = "*", verbose = verbose))
      notUsed <- NULL
    }
  }#end for each year
}

pisaURLDat <- function(year, database = "INT") {
  text <- "year	database	type	url
2012	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_STU12_DEC03.zip
2012	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_SCQ12_DEC03.zip
2012	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_PAQ12_DEC03.zip
2012	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_COG12_DEC03.zip
2012	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_COG12_S_DEC03.zip
2012	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_student.txt
2012	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_school.txt
2012	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_parent.txt
2012	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_cognitive_item.txt
2012	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_scored_cognitive_item.txt
2012	CBA	data	http://www.oecd.org/pisa/pisaproducts/CBA_STU12_MAR31.zip
2012	CBA	data	http://www.oecd.org/pisa/pisaproducts/CBA_SCQ12_MAR31.zip
2012	CBA	data	http://www.oecd.org/pisa/pisaproducts/CBA_PAQ12_MAR31.zip
2012	CBA	data	http://www.oecd.org/pisa/pisaproducts/CBA_COG12_MAR31.zip
2012	CBA	data	http://www.oecd.org/pisa/pisaproducts/CBA_COG12_S_MAR31.zip
2012	CBA	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_CBA_student.txt
2012	CBA	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_CBA_school.txt
2012	CBA	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_CBA_parent.txt
2012	CBA	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_CBA_cognitive_item.txt
2012	CBA	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_CBA_scored_cognitive_item.txt
2012	FIN	data	http://www.oecd.org/pisa/pisaproducts/FIN_STU12_MAR31.zip
2012	FIN	data	http://www.oecd.org/pisa/pisaproducts/FIN_SCQ12_MAR31.zip
2012	FIN	data	http://www.oecd.org/pisa/pisaproducts/FIN_PAQ12_MAR31.zip
2012	FIN	data	http://www.oecd.org/pisa/pisaproducts/FIN_COG12_MAR31.zip
2012	FIN	data	http://www.oecd.org/pisa/pisaproducts/FIN_COG12_S_MAR31.zip
2012	FIN	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_FIN_student.txt
2012	FIN	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_FIN_school.txt
2012	FIN	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_FIN_parent.txt
2012	FIN	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_FIN_cognitive_item.txt
2012	FIN	spss	http://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_FIN_scored_cognitive_item.txt
2009	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_STQ09_DEC11.zip
2009	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_SCQ09_Dec11.zip
2009	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_PAR09_DEC11.zip
2009	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_COG09_TD_DEC11.zip
2009	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_COG09_S_DEC11.zip
2009	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2009_SPSS_student.txt
2009	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2009_SPSS_school.txt
2009	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2009_SPSS_parent.txt
2009	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2009_SPSS_cognitive_item.txt
2009	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2009_SPSS_score_cognitive_item.txt
2006	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_Stu06_Dec07.zip
2006	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_Sch06_Dec07.zip
2006	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_Par06_Dec07.zip
2006	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_Cogn06_T_Dec07.zip
2006	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_Cogn06_S_Dec07.zip
2006	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2006_SPSS_student.txt
2006	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2006_SPSS_school.txt
2006	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2006_SPSS_parent.txt
2006	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2006_SPSS_cognitive_item.txt
2006	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2006_SPSS_scored_cognitive_item.txt
2003	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_cogn_2003.zip
2003	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_stui_2003_v2.zip
2003	INT	data	http://www.oecd.org/pisa/pisaproducts/INT_schi_2003.zip
2003	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2003_SPSS_cognitive_item.txt
2003	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2003_SPSS_student.txt
2003	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2003_SPSS_school.txt
2000	INT	data	http://www.oecd.org/pisa/pisaproducts/intcogn_v4.zip
2000	INT	data	http://www.oecd.org/pisa/pisaproducts/intscho.zip
2000	INT	data	http://www.oecd.org/pisa/pisaproducts/intstud_math.zip
2000	INT	data	http://www.oecd.org/pisa/pisaproducts/intstud_read.zip
2000	INT	data	http://www.oecd.org/pisa/pisaproducts/intstud_scie.zip
2000	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2000_SPSS_cognitive_item.txt
2000	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2000_SPSS_school_questionnaire.txt
2000	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2000_SPSS_student_mathematics.txt
2000	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2000_SPSS_student_reading.txt
2000	INT	spss	http://www.oecd.org/pisa/pisaproducts/PISA2000_SPSS_student_science.txt
"
  urlDat <- do.call("rbind", strsplit(unlist(strsplit(text,"\n")),"\t"))
  urlDat <- data.frame(urlDat, stringsAsFactors = FALSE)
  colnames(urlDat) <- urlDat[1,]
  urlDat <- urlDat[urlDat$year %in% year & urlDat$database %in% database,]
  return(urlDat$url)
}
