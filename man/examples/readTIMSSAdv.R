\dontrun{
swe <- readTIMSSAdv("C:/TIMSSAdvanced/Math/2015",
                    countries = c("swe"), subject = "math")
gg <- getData(swe, c("itsex", "totwgt", "malg"))
head(gg)
edsurveyTable(malg ~ itsex, swe)
}
