skip_on_cran()
require(testthat)
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)

source("REF-mixed.R")

if(!exists("edsurveyHome")) {
  if (Sys.info()[['sysname']] == "Windows") {
    edsurveyHome <- "C:/EdSurveyData/"
  } else {
    edsurveyHome <- "~/EdSurveyData/"
  }
}

context('mixed.sdf')
test_that('mixed.sdf', {
  usa8 <- readTIMSS(paste0(edsurveyHome, "TIMSS/2003"), countries = c("usa"), gradeLvl = 8, verbose=FALSE)
  usa8dat <- getData(data=usa8, c("mmat", "idclass", "totwgt", "schwgt","itsex"), returnJKreplicates=FALSE, addAttributes=TRUE)
  usa8dat$wsum <- ave(usa8dat$totwgt, usa8dat$idclass, FUN=mean)
  usa8dat$w1 <- usa8dat$totwgt / usa8dat$wsum * usa8dat$schwgt
  usa8dat$w2 <- usa8dat$schwgt
  system.time(m1 <- mixed.sdf(mmat ~ itsex + (1|idclass), data=usa8dat, weightVars=c("w1", "w2"), weightTransformation=FALSE, verbose=FALSE))
  withr::with_options(list(digits=4),
                      co <- capture.output(summary(m1)))
  expect_equal(co, m1SummaryREF)
})
