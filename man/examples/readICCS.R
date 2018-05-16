\dontrun{
eng <- readCivEDICCS("C:/ICCS2009/Gr8", countries = c("eng"),
                     gradeLvl = 8, dataSet = "student")
gg <- getData(eng, c("famstruc", "totwgts", "civ"))
head(gg)
edsurveyTable(civ ~ famstruc, eng)
}
