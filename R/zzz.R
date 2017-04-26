# @author Paul Bailey
.onAttach <- function(libname, pkgname) {
	packageStartupMessage(paste0("EdSurvey v", utils::packageDescription("EdSurvey")$Version, "\n"))
}

globalVariables(c("variable", "w"))
