\dontrun{
nor <- readPIRLS("C:/PIRLS2011", countries = c("nor"))
gg <- getData(nor, c("itsex", "totwgt", "rrea"))
head(gg)
edsurveyTable(rrea ~ itsex, nor)
}
