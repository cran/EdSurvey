# @author Paul Bailey
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("EdSurvey v", utils::packageDescription("EdSurvey")$Version, "\n"))
}

globalVariables(c("variable", "w", "c2", "dofNum", "dofDenom", "lengthY",
                  "sumY", "Percent", "wtdN", "N", "pcti", "value", "Vjrr",
                  "Vimp", "Level", "JKreplicate", "PV", "V1", "cnt_index",
                  "w_lmi", "StandardError", "flag1"))
