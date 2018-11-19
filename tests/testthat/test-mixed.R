skip_on_cran()
require(testthat)
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
sdf_subset <- subset(sdf, scrpsu < 500)

mixed1REF <- c("Call:", "mixed.sdf(formula = composite ~ dsex + b017451 + (1 | scrpsu), ", 
               "    data = sdf_subset, weightVars = c(\"origwt\", \"srwt01\"), nQuad = 5, ", 
               "    verbose = 0, fast = TRUE)", "", "Formula: composite ~ dsex + b017451 + (1 | scrpsu)", 
               "", "", "Plausible Values:  5", "Number of Groups: ", "  Group Var Observations Level", 
               "1    scrpsu           22     2", "2  Residual          492     1", 
               "", "Variance terms:", "                   variance Std. Error Std.Dev.", 
               "scrpsu:(Intercept)      626        199       25", "Residual                883        109       30", 
               "", "Fixed Effects:", "                            Estimate Std. Error t value", 
               "(Intercept)                   273.52       7.79   35.12", "dsexFemale                      1.17       3.97    0.30", 
               "b017451Once every few weeks    -1.54       9.48   -0.16", "b017451About once a week        9.04       5.48    1.65", 
               "b0174512 or 3 times a week      6.21       7.43    0.84", "b017451Every day                3.40       8.55    0.40", 
               "", "Intraclass Correlation= 0.415 ")

mixed2REF <- c("Call:", "mixed.sdf(formula = I(composite >= 214) ~ (1 | scrpsu), data = sdf_subset, ", 
               "    weightVars = c(\"origwt\", \"srwt01\"), nQuad = 5, verbose = 0, ", 
               "    family = binomial(link = \"logit\"))", "", "Formula: I(composite >= 214) ~ (1 | scrpsu)", 
               "", "", "Plausible Values:  5", "Number of Groups: ", "  Group Var Observations Level", 
               "1    scrpsu           22     2", "", "Variance terms:", "                   variance Std. Error Std.Dev.", 
               "scrpsu:(Intercept)      2.3        2.2      1.5", "", "Fixed Effects:", 
               "            Estimate Std. Error t value", "(Intercept)    4.100      0.998    4.11"
)

context('mixed.sdf')
test_that('mixed.sdf', {
  expect_warning(m1 <- mixed.sdf(composite ~ dsex + b017451 + (1|scrpsu), data=sdf_subset,
                  weightVar = c('origwt', 'srwt01'),fast=TRUE, verbose=0, nQuad=5))
  options(digits=2)
  m1c <- capture.output(summary(m1))
  expect_equal(m1c,mixed1REF)
})


context('mixed.sdf logit')
test_that('mixed.sdf logit', {
  expect_warning(m2 <- mixed.sdf(I(composite >= 214) ~ (1|scrpsu), 
                  data=sdf_subset, family = binomial(link="logit"),
                  weightVar = c('origwt', 'srwt01'), 
                  nQuad = 5, verbose=0))
  options(digits=2)
  m2c <- capture.output(summary(m2))
  expect_equal(m2c, mixed2REF)
})