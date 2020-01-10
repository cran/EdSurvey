\dontrun{
# root argument will vary by operating system conventions
download_ePIRLS(years=2016, root = "C:/")

# cache=TRUE will download then process the datafiles
download_ePIRLS(years=2016, root = "C:/", cache = TRUE)

# set verbose=FALSE for silent output
# if year not specified, download all years
download_ePIRLS(root="C:/", verbose = FALSE)
}
