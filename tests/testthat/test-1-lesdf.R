# This file tests that the light.edsurvey.data.frame works 
# the same as the edsurvey.data.frame
require(testthat)
require(EdSurvey)
# if(interactive()) {
#  setwd(system.file("testRDs", package = "EdSurvey"))
# }
options(width = 500)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
lsdf <- getData(sdf, c(all.vars(composite ~ dsex + b017451), "origwt"), addAttributes=TRUE)


context("LESDF cbind function")
test_that("LESDF cbind function",{
  skip_on_cran()
  sm1 <- getData(sdf, c('composite', 'dsex', 'origwt'), dropUnusedLevels=FALSE, defaultConditions=FALSE, omittedLevels=FALSE, addAttributes=TRUE)
  sm2 <- getData(sdf, c('b017451'), dropUnusedLevels=FALSE, defaultConditions=FALSE, omittedLevels=FALSE)
  sm3 <- cbind(b017451=sm2$b017451,sm1)
  sm4 <- getData(sdf, c('composite','b017451', 'dsex', 'origwt'), dropUnusedLevels=FALSE, defaultConditions=FALSE, omittedLevels=FALSE, addAttributes=TRUE)
  expect_equal(attributes(sm4)$names,attributes(sm3)$names) # test just attribute names
  expect_equal(sm4,sm3) # test everything
})

context("getData addAttributesTRUE returns a LESDF")
test_that("getData addAttributesTRUE returns a LESDF", {
	expect_is(sdf, "edsurvey.data.frame")
	expect_is(lsdf, "light.edsurvey.data.frame")
})

context("getData ignores defaultConditions when applied twice")
test_that("getData ignores defaultConditions when applied twice", {
  skip_on_cran()
  lsdf1 <- getData(sdf, c("composite", "dsex", "b017451", "origwt"), addAttributes=TRUE, defaultConditions=FALSE)
  expect_equal(lsdf1, suppressWarnings(lsdf2 <- getData(lsdf1, c("composite", "dsex", "b017451", "origwt"))))
  expect_equal(lsdf1, suppressWarnings(lsdf3 <- getData(lsdf1, c("composite", "dsex", "b017451", "origwt"), defaultConditions=FALSE)))
  expect_equal(lsdf1, suppressWarnings(lsdf4 <- getData(lsdf1, c("composite", "dsex", "b017451", "origwt"), defaultConditions=TRUE)))
  lsdf2 <- getData(sdf, c("composite", "dsex", "b017451", "origwt"), addAttributes=TRUE, defaultConditions=TRUE)
  expect_equal(lsdf2, suppressWarnings(lsdf2 <- getData(lsdf2, c("composite", "dsex", "b017451", "origwt"))))
  expect_equal(lsdf2, suppressWarnings(lsdf3 <- getData(lsdf2, c("composite", "dsex", "b017451", "origwt"), defaultConditions=FALSE)))
  expect_equal(lsdf2, suppressWarnings(lsdf4 <- getData(lsdf2, c("composite", "dsex", "b017451", "origwt"), defaultConditions=TRUE)))
})


context("LESDF subset")
test_that("LESDF subset",{
  skip_on_cran()
  s1 <- subset(lsdf, dsex == "Male", verbose=FALSE)
  expect_equal(s2 <- base::subset(lsdf, dsex == "Male"),s1)

  sdfb12 <- getData(subset(sdf, b017451 %in% c(1,2), verbose=FALSE), c("b017451", "dsex"), dropUnusedLevels=FALSE)
  lsdfb12 <- subset(getData(sdf, c("b017451", "dsex"), dropUnusedLevels=FALSE), b017451 %in% c(1,2))
  expect_equal(sdfb12, lsdfb12)

  sdfb12 <- getData(subset(sdf, b017451 %in% c("Never or hardly ever","Once every few weeks"), verbose=FALSE), c("b017451", "dsex"), dropUnusedLevels=FALSE)
  lsdfb12 <- subset(getData(sdf, c("b017451", "dsex"), dropUnusedLevels=FALSE), b017451 %in% c("Never or hardly ever","Once every few weeks"))
  expect_equal(sdfb12, lsdfb12)

  s2 <- subset(lsdf, dsex == "Male", verbose=FALSE)
  expect_equal(attributes(s2)$names,attributes(lsdf)$names)
})

context("LESDF getData warnings")
test_that("LESDF getData warnings",{
  skip_on_cran()
  co <-evaluate_promise(getData(sdf, c('composite','dsex', 'b017451', 'origwt'), dropUnusedLevels = FALSE, defaultConditions = FALSE, addAttributes = TRUE, omittedLevels = FALSE))
  expect_equal(unique(co$warnings),character(0))
  expect_warning(co <- getData(sdf,
  	                           c('composite','dsex', 'b017451', 'm144901', 'origwt'),
  	                           dropUnusedLevels = FALSE,
  	                           defaultConditions = FALSE,
  	                           addAttributes = TRUE,
  	                           omittedLevels = FALSE), 
                 paste("Updating labels on ",sQuote('m144901')," because there are multiples of the label ", sQuote("Correct"),".",sep =""))
})

context("LESDF Simple functions")
test_that("hasPlausibleValue", {
  skip_on_cran()
  expect_true(hasPlausibleValue("composite", sdf))
  expect_true(hasPlausibleValue("composite", lsdf))
  expect_false(hasPlausibleValue("dsex", sdf))
  expect_false(hasPlausibleValue("dsex", lsdf))
  expect_equal(hasPlausibleValue("composite", sdf), hasPlausibleValue("composite", lsdf))
})

test_that("getPlausibleValue", {
  skip_on_cran()
  expect_equal(getPlausibleValue("composite", sdf), getPlausibleValue("composite", lsdf))
  expect_error(getPlausibleValue("dsex", sdf))
  expect_error(getPlausibleValue("dsex", lsdf))
})

test_that("getWeightJkReplicates", {
  skip_on_cran()
  expect_equal(getWeightJkReplicates("origwt", sdf), getWeightJkReplicates("origwt", lsdf))
  expect_error(getWeightJkReplicates("composite", sdf))
  expect_error(getWeightJkReplicates("composite", lsdf))
})

test_that("isWeight", {
  skip_on_cran()
  expect_true(isWeight("origwt", sdf))
  expect_true(isWeight("origwt", lsdf))
  expect_false(isWeight("composite", sdf))
  expect_false(isWeight("composite", lsdf))
  expect_equal(isWeight("origwt", sdf), isWeight("origwt", lsdf))
})

context("LESDF lm.sdf")
test_that("LESDF lm.sdf",{
  skip_on_cran()
	sdfoutput <- capture.output(print(sm1 <- summary(lm.sdf(composite ~ dsex + b017451,sdf, jrrIMax=Inf))))
	gdoutput <- capture.output(print(sm2 <- summary(lm.sdf(composite ~ dsex + b017451,lsdf, jrrIMax=Inf))))
	expect_equal(sdfoutput, gdoutput)
	# do not expect the calls to be the same
	sm1$call <- NULL
  sm2$call <- NULL
	expect_equal(sm1, sm2)
})

context("LESDF print")
test_that("LESDF print",{
  skip_on_cran()
	sdfoutput <- capture.output(print(sm1 <- lm.sdf(composite ~ dsex + b017451,sdf, jrrIMax=Inf)))
	gdoutput <- capture.output(print(sm2 <- lm.sdf(composite ~ dsex + b017451,lsdf, jrrIMax=Inf)))
	expect_equal(gdoutput, sdfoutput)
	# do not expect the calls to be the same
	sm1$call <- NULL
  sm2$call <- NULL
	expect_equal(sm1, sm2)
})

context("LESDF edsurveyTable")
test_that("LESDF edsurveyTable",{
  skip_on_cran()
	expect_equal_to_reference(es10 <- edsurveyTable( ~ dsex + b017451, lsdf, jrrIMax=1), file="es1.rds")
	# two levels, results checked vs Primer
	expect_equal_to_reference(es1 <- edsurveyTable(composite ~ dsex + b017451, lsdf, jrrIMax=1), file="es1.rds")
	# check for just males (dsex is only occupied at one level)
	lsdfm <- subset(lsdf, dsex=="Male")
	expect_equal_to_reference(es2l <- edsurveyTable(composite ~ dsex + b017451, lsdfm, jrrIMax=Inf), file="es2.rds")
	# test omittedLevels, here it should be ignored and es2 is the correct reference
	expect_equal_to_reference(suppressWarnings(es2lb <- edsurveyTable(composite ~ dsex + b017451, lsdfm, jrrIMax=Inf, omittedLevels=FALSE)), file="es2.rds")    # this is just a change test, not a confirmed correct test
})

context("LESDF lm.sdf")
test_that("LESDF lm.sdf",{
  skip_on_cran()
	sm1 <- getData(sdf, c(all.vars(composite ~ dsex + b017451), "origwt"), addAttributes=TRUE)
	sm1 <- subset(sm1, dsex == "Male")
	sm1 <- subset(sm1, dsex == "Female")
	expect_error(lm.sdf(composite ~ dsex + b017451,sm1, jrrIMax=Inf))
})

context("LESDF lm.sdf correctly returns errors")
test_that("LESDF lm.sdf correctly returns errors",{
  skip_on_cran()
	sm1 <- getData(sdf, c(all.vars(composite ~ dsex + b017451), "origwt"), addAttributes=TRUE)
	# no error with relevel calls 
	expect_is(lm.sdf(composite ~ dsex + b017451, relevels = list(dsex="Male"),sm1, jrrIMax=Inf), "edsurveyLm")
	expect_is(lm.sdf(composite ~ dsex + b017451, relevels = list(b017451="Once every few weeks"),sm1, jrrIMax=Inf), "edsurveyLm")
	sm2 <- subset(sm1, dsex == "Male")
	sm2 <- subset(sm2, b017451 != "Once every few weeks")
	expect_error(lm.sdf(composite ~ dsex + b017451, relevels = list(dsex="Male"), sm2, jrrIMax=Inf))
	expect_error(lm.sdf(composite ~ dsex + b017451, relevels = list(b017451="Once every few weeks"), sm2, jrrIMax=Inf))
})

test_that("lm.sdf function returns error when variable not in getData call",{
  skip_on_cran()
	sm1 <- getData(sdf, c(all.vars(composite ~ dsex + b017451), "origwt"), addAttributes=TRUE)
	expect_error(lm.sdf(composite ~ dsex + b017451 + iep,sm1, jrrIMax=Inf))
})

context("LESDF cor.sdf")
test_that("LESDF cor.sdf",{
  skip_on_cran()
  b1 <- cor.sdf("m815401", "b017451",method="Pearson", sdf,weightVar = "origwt")
  lsdf <- getData(sdf,c("m815401", "b017451","origwt"), addAttributes=TRUE, omittedLevels = TRUE)
  b2 <- cor.sdf("m815401", "b017451",method="Pearson", lsdf,weightVar = "origwt")
  expect_equal(b1,b2)
  skip_on_cran()

  b3 <- cor.sdf("m815401", "b017451",method="Pearson", sdf,weightVar = "origwt")
  lsdf2 <- getData(sdf,c("m815401","m815701", "b017451","origwt"), addAttributes=TRUE, omittedLevels = FALSE)
  b4 <- cor.sdf("m815401", "b017451",method="Pearson", lsdf2,weightVar = "origwt", omittedLevels=TRUE) # dropUnusedLevels nolonger revealed, not set
  expect_equal(b3,b4)
  expect_equal(b2,b4)
} )

context("LESDF achievementLevels")
test_that("LESDF achievementLevels",{
  skip_on_cran()
  lsdf1l <- getData(sdf, c("composite", "origwt"), addAttributes=TRUE)
  expect_equal_to_reference(test1l <- achievementLevels(returnCumulative = TRUE, data=lsdf1l), file="aLevels_test1.rds")
  a1 <- achievementLevels(c("composite","dsex", "b017451"),
                          aggregateBy = "dsex", sdf,
                          recode=list(
                                 b017451=list(
                                         from=c("Never or hardly ever",
                                         "Once every few weeks","About once a week"),
                                         to=c("Infrequently")),
                                 b017451=list(
                                         from=c("2 or 3 times a week","Every day"),
                                         to=c("Frequently"))))
  a2 <- achievementLevels(c("composite","dsex", "b017451"),
                          aggregateBy = "dsex", lsdf,
                          recode=list(
                                 b017451=list(
                                         from=c("Never or hardly ever",
                                         "Once every few weeks","About once a week"),
                                         to=c("Infrequently")),
                                 b017451=list(
                                         from=c("2 or 3 times a week","Every day"),
                                         to=c("Frequently"))))
  expect_equal(a1, a2)
})
