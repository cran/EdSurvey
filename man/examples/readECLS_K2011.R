\dontrun{
#read-in student file with defaults
eclsk_df <- readECLS_K2011(path="C:/ECLS_K/2011") #using defaults
d <- getData(eclsk_df, c("childid", "c1hgt1", "c1wgt1"))
summary(d)

#read-in with parameters specified
eclsk_df <- readECLS_K2011(path = "C:/ECLS_K2011",
                           filename = "childK4p.dat",
                           layoutFilename = "ECLSK2011_K4PUF.sps",
                           forceReread = FALSE,
                           verbose = TRUE) 
}
