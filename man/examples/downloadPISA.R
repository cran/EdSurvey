\dontrun{
# Download PISA 2012 data (for all three databases)
downloadPISA(years = 2012, database = c("INT","CBA","FIN"), root="C:/")

# Download PISA 2009 and 2012 data (International Database only) 
# to C:/PISA/2009 and C:/PISA/2012 folder respectively
downloadPISA(years = c(2009,2012), root="C:/")  
}
