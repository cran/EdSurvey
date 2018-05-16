\dontrun{
# The following call returns an edsurvey.data.frame to PIAAC for Canada
can <- readPIAAC("C:/PIAAC", countries = "can")

# Extract a data.frame with a few variables
gg <- getData(can, c("c_d05","ageg10lfs"))  
head(gg)

# Conduct a preliminary analysis on the edsurvey.data.frame
edsurveyTable(~ c_d05 + ageg10lfs, data = can)
}

