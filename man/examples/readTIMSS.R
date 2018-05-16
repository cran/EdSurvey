\dontrun{
#single country specified
fin <- readTIMSS("C:/TIMSS2015", countries = c("fin"), gradeLvl = 4)
gg <- getData(fin, c("asbg01", "totwgt", "srea"))
head(gg)
edsurveyTable(srea ~ asbg01, fin)

#multiple countries returned as edsurvey.data.frame.list, specify all countries with '*' argument
timss2011 <- readTIMSS("C:/TIMSS2011", countries="*", gradeLvl = 8, verbose = TRUE)
#print out edsurvey.data.frame.list covariates
timss2011$covs
}