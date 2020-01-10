\dontrun{
# read-in student file with defaults
eclsk_df <- readECLS_K1998(path="C:/ECLS_K/1998") #using defaults
d <- getData(eclsk_df, c("childid", "gender", "race"))
summary(d)

# read-in with parameters specified
eclsk_df <- readECLS_K1998(path = "C:/ECLS_K1998", 
                           filename = "eclsk_98_99_k8_child_v1_0.dat", 
                           layoutFilename = "Layout_k8_child.txt", 
                           verbose = TRUE, 
                           forceReread = FALSE)
}
