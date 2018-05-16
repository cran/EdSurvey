\dontrun{
# The following call returns an edsurvey.data.frame to TALIS 2013 
# for US teacher-level data at secondary level
usa2013 <- readTALIS(path = "C:/TALIS/2013", isced = "b",
                     dataLevel = "teacher", countries = "usa")

# Extract a data.frame with a few variables
gg <- getData(usa2013, c("tt2g05b", "tt2g01"))  
head(gg)

# Conduct a preliminary analysis on the edsurvey.data.frame
edsurveyTable(tt2g05b ~ tt2g01, data = usa2013) 
}
