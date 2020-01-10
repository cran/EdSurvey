\dontrun{
# root argument will vary by operating system conventions
downloadECLS_K(years=c(1998, 2011), root = "C:/")

# cache=TRUE will download then process the datafiles
downloadECLS_K(years=c(1998, 2011), root = "C:/", cache = TRUE)

# set verbose=FALSE for silent output
# if year not specified, download all years
downloadECLS_K(root="C:/", verbose = FALSE)
}
