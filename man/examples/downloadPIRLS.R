\dontrun{
  # root argument will vary by operating system conventions
  downloadPIRLS(year=c(2006, 2011), root = "C:/")
  
  # cache=TRUE will download then process the datafiles
  downloadPIRLS(year=2011, root = "C:/", cache = TRUE)
}
