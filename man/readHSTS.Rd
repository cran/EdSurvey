% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readHSTS.R
\name{readHSTS}
\alias{readHSTS}
\title{Connect to HSTS Data}
\usage{
readHSTS(
  dataFilePath = getwd(),
  spssPrgPath = dataFilePath,
  year = c("2019"),
  verbose = TRUE
)
}
\arguments{
\item{dataFilePath}{a character value to the root directory path of extracted set of ASCII data files (.txt or .dat file extension).
\code{readHSTS} will search within sub-directories of this parameter for expected data files based on the specified \code{year} parameter.}

\item{spssPrgPath}{a character value to the directory path of where the extracted set of .sps program files are located.
The data file and associated SPSS program filenames *must match* (having different file extensions) to determine which files are associated together.
\code{readHSTS} will search within sub-directories of this parameter for expected SPSS programe files based on the specified \code{year} parameter.}

\item{year}{a character value to indicate the year of the dataset.  Only one year is supported for a single \code{readHSTS} data call.
The year is required to help determine specific study information.  Only 2019 study is currently supported.}

\item{verbose}{a logical value that will determine if you want verbose output while the \code{readHSTS} function is running to indicate processing progress.
The default value is \code{TRUE}.}
}
\value{
an \code{edsurvey.data.frame} for the HSTS dataset.
}
\description{
Opens a connection to a High School Transcript Study (HSTS) data files for years 2019.
             Returns an \code{edsurvey.data.frame} with
             information about the file and data.
}
\details{
The HSTS data has a complex structure and unique characteristics all handled internally within \code{EdSurvey}. 
          The structure allows for automatic dynamic linking across all various data 'levels' based the requested variables. The \code{student} data level is the primary analysis unit. 
          Dynamic linking for variables that include both \code{tests} and \code{transcript} level details will result in an error, as they cannot be simultaneously returned in a single call. 
          Situations may arise where the analyst must derive variables for analysis. See the documentation for \code{merge} and \code{$<-} functions for more detail. All merge operations are done at the \code{student} level (the main analysis unit).
           
File Layout for HSTS 2019:
 \itemize{
       \item School (school.dat) - School level variables.
       \itemize{
         \item School Catalog (catalog.dat) - Catalog variables joined to School data. Variables renamed to begin with \code{SchCat_} to distinguish from Transcript Catalog.  Cannot be merged with any \code{Student} data.
       }
       \item Student (student.dat) - Student level variables.  Primary analysis unit, all merged/cached data must be at this level.
       \itemize{
         \item NAEP Math (naepmath.dat) - Subset of students containing NAEP Math variables.  Variables begin with \code{math_} to ensure they are unique from the NAEP Science variables.
         \item NAEP Science (naepsci.dat) - Subset of students containing NAEP Science variables.  Variables begin with \code{sci_} to ensure they are unique from the NAEP Math variables.
         \item Tests (tests.dat) - Students may have many test records.  Contains ACT/SAT testing score details for students.  Cannot be merged together with any Transcript or Transcript Catalog data.
         \item Transcripts (trnscrpt.dat) - Students may have many transcript records.  Contains transcript level details. Cannot be merged together with Test data.
           \itemize{
             \item Transcript Catalog (catalog.dat) - Each transcript record is associated to a catalog record for giving context to the transcript record.  2019 uses \href{https://nces.ed.gov/scedfinder}{SCED codes} for categorizing courses.
           }
       }
 }
}
\seealso{
\code{\link{showCodebook}}, \code{\link{searchSDF}}, \code{\link{edsurvey.data.frame}}, \code{\link{merge.edsurvey.data.frame}}, and \code{\link{getData}}
}
\author{
Tom Fink
}
