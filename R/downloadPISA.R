#' @title Download and Unzip PISA Files
#'
#' @description Uses an Internet connection to download PISA data to a
#'              computer. Data come from the OECD website. 
#' 
#' @param root a character string indicating the directory where the PISA data should
#'             be stored. Files are placed in a folder named PISA/[year].
#' @param years an integer vector of the assessment years to download. Valid years are 2000, 2003,
#'              2006, 2009, 2012, and 2015.
#' @param database a character vector to indicate which database to download from. For 2012,
#'              three databases are available (\code{INT} = International, \code{CBA} = Computer-Based Assessment, and
#'              \code{FIN} = Financial Literacy). For other years, only \code{INT} is available (for example, if PISA 
#'              2015 financial literacy is to be downloaded, the database argument should be set to \code{INT}).
#'              Defaults to \code{INT}.
#' @param cache a logical value set to process and cache the text (.txt) version of files.
#'              This takes a very long time but saves time for future uses of 
#'              the data. Default value is \code{FALSE}.
#' @param verbose a logical value to either print or suppress status message output.
#'                The default value is \code{TRUE}.
#' @details
#' The function uses
#' \ifelse{latex}{\code{download.file}}{\code{\link[utils]{download.file}}}
#' to download files from provided URLs. Some machines might require a different
#' user agent in HTPP(S) requests. If the downloading gives an error or behaves
#' unexpectedly (e.g., a zip file cannot be unzipped or a data file is
#' significantly smaller than expected), users can toggle \code{HTTPUserAgent}
#' options to find one that works for their machines. One common alternative option is 
#'
#' \code{options(HTTPUserAgent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0")}
#' 
#' @seealso \code{\link{readPISA}}, \ifelse{latex}{\code{download.file}}{\code{\link[utils]{download.file}}}, \ifelse{latex}{\code{options}}{\code{\link[base]{options}}}
#' @author Yuqi Liao, Paul Bailey, and Trang Nguyen
#' @example man/examples/downloadPISA.R
#' @importFrom utils unzip
#' @export
downloadPISA <- function(root, years=c(2000, 2003, 2006, 2009, 2012, 2015), database=c("INT","CBA","FIN"), cache=FALSE, verbose=TRUE) {
  # valid years for PISA
  validYears <- c(2000, 2003, 2006, 2009, 2012, 2015)
  years <- as.numeric(years)
  if (missing(database)) {
    # if database is not specified, default to be INT because usually users do not want to download all databases
    database <- "INT"
  }
  for (y in years) {
    if(verbose) {
      cat(paste0("\nProcessing PISA data for year ", y, "\n"))
    }
    if (!y %in% validYears) {
      warning(sQuote(y), " is not a valid year. PISA had data for the following year: ", paste0(validYears, sep = " "))
      next
    }
    
    # Create a year root directory
    baseroot <- file.path(root,"PISA/")
    if(!dir.exists(baseroot)) {
      dir.create(baseroot)
    }
    yroot <- file.path(baseroot,y)
    if(!dir.exists(yroot)) {
      dir.create(yroot)
    }
    
    # Download all files
    for (d in database) {
      collected_files <- pisaURLDat(y,d)
      if (length(collected_files) == 0) {
        warning("Database ",sQuote(d), " is not available for year ", sQuote(y))
        next
      }
      if(verbose) {
        cat(paste0("Database ",d,"\n"))
      }
      for (f in collected_files) {
        fn <- basename(f)
        if(!file.exists(file.path(yroot,fn))) {
          #options(HTTPUserAgent="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0")
          if(grepl("http",f, ignore.case = T)) {
            download.file(f,file.path(yroot,fn),quiet = !verbose)
          } else {
            download.file(paste0("http://www.oecd.org/",f),file.path(yroot,fn), quiet = !verbose)
          }
        } else {
          if (verbose) {
            cat(paste0("Found downloaded ",y," PISA (",d," database) file ",fn,".\n"))
          }
        }
      }
      # Unzipping files
      zFiles <- list.files(yroot,pattern = "\\.zip$", ignore.case = T, full.names = F)
      zFiles <- file.path(yroot,zFiles)
      for (z in zFiles) {
        lst <- tryCatch(unzip(z, list = TRUE),
                        error = function(cond) {
                          message("File downloading for ",z," does not work properly. Users might need to change HTTPUserAgent option. See ?downloadPISA for more information.")
                          stop(cond)
                        })
        if(verbose) {
          cat(paste0("Unzipping ",y," PISA (",d," database) files from ",z,"\n"))
        }
        for(i in 1:nrow(lst)) {
          if(!file.exists(file.path(yroot,basename(lst$Name[i]))) | file.info(file.path(yroot,basename(lst$Name[i])))$size != lst$Length[i]) {
            if (verbose) {
              cat(paste0(" unzipping ",lst$Name[i],"\n"))
            }
            unzip(z,files=lst$Name[i], exdir = yroot)
                    
            if(basename(lst$Name[i]) != lst$Name[i]) {
              file.rename(file.path(yroot, lst$Name[i]), file.path(yroot, basename(lst$Name[i])))
            }
          }
        }
      }
      # Process files if required
      if (cache) {
        suppressWarnings(notUsed <- readPISA(yroot, database = d, countries = "*", verbose = verbose))
        notUsed <- NULL
      }
    } #end for each database
  }#end for each year
}

pisaURLDat <- function(year, database = "INT") {
  text <- "year	database	type	url
2015	INT	data	http://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QQQ.zip
2015	INT	data	http://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_SCH_QQQ.zip
2015	INT	data	http://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_COG.zip
2015	INT	data	http://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QTM.zip
2015	INT	data	http://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_FLT.zip
2015	INT	data	http://webfs.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_CPS.zip
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
