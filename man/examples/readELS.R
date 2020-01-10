\dontrun{
# read-in student file including weight file as default
setwd("C:/ELS2002")
els_df <- readELS() #student level with weights)
d <- getData(els_df, c("stu_id", "bysex", "bystlang"))
summary(d)

# read-in with parameters specified (student level with weights)
els_wgt_df <- readELS(path = "C:/ELS2002", 
                            filename = "els_02_12_byf3pststu_v1_0.dat", 
                            layoutFilename = "Layout_BYF3PSTSTU.PRI.txt", 
                            wgtFilename = "els_02_12_byf3stubrr_v1_0.dat",
                            wgtLayoutFilename = "Layout_BYF3BRRPST.PRI.txt",
                            verbose = TRUE, 
                            forceReread = FALSE)

# read-in with parameters specified (school level, no weights)
els_sch_df <- readELS(path = "C:/ELS2002", 
                      filename = "els_02_12_byf1sch_v1_0.dat", 
                      layoutFilename = "Layout_BYF1SCH.txt", 
                      wgtFilename = NA,
                      wgtLayoutFilename = NA,
                      verbose = TRUE, 
                      forceReread = FALSE)

}
