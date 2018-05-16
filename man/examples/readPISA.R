\dontrun{
# The following call returns an edsurvey.data.frame to 
# PISA 2012 International Database for Singapore
sgp2012 <- readPISA(path = "C:/PISA/2012", database = "INT", countries = "sgp")

# Extract a data.frame with a few variables
gg <- getData(sgp2012, c("cnt","read","w_fstuwt"))  
head(gg)

# Conduct a preliminary analysis on the edsurvey.data.frame
edsurveyTable(read ~ st04q01 + st20q01, data = sgp2012)
}

