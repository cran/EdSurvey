% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadHSLS.R
\name{downloadHSLS}
\alias{downloadHSLS}
\title{Download and Unzip HSLS Files}
\usage{
downloadHSLS(root, years = c(2009), cache = FALSE, verbose = TRUE)
}
\arguments{
\item{root}{a character string indicating the directory where the HSLS
data should be stored. Files are placed in a
subdirectory named HSLS/[year].}

\item{years}{an integer vector of the assessment years to download. Valid year is 2009 only.}

\item{cache}{a logical value set to process and cache the text (.txt) version of files.
This takes a very long time but saves time for future uses of
the data. Default value is \code{FALSE}.}

\item{verbose}{a logical value to either print or suppress status message output.
The default value is \code{TRUE}.}
}
\description{
Uses an Internet connection to download HSLS data.
             Data come from \href{https://nces.ed.gov/edat/}{nces.ed.gov} zip files. This
             function works for 2009 data.
}
\examples{
\dontrun{
# root argument will vary by operating system conventions
downloadHSLS(root = "~/", years=2009)

# set verbose=FALSE for silent output
# if year not specified, download all years
downloadHSLS(root="~/", verbose = FALSE)
}
}
\seealso{
\code{\link{readHSLS}}
}
\author{
Tom Fink
}
