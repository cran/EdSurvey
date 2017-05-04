require(testthat)
require(EdSurvey)
options(width = 500)
options(useFancyQuotes=FALSE)

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))


context("wd is set correctly") #When this fails all regression tests are invalid.
test_that("wd is set correctly",{
  skip_on_cran()
  expect_is(es1_ <- readRDS("regression.rds"), "data.frame")
})

context("Primer reads in correctly")
test_that("Primer reads in correctly", {
  expect_is(sdf, "edsurvey.data.frame")
  expect_equal(dim(sdf), c(17606, 302))
  expect_equal(c(nrow(sdf), ncol(sdf)), c(17606, 302))
})

context("showPlausibleValues and showWeights verbose output agrees")
test_that("showPlausibleValues and showWeights verbose output agrees",{
  skip_on_cran()
  spv <- c("There are 6 subject scale(s) or subscale(s) in this edsurvey.data.frame", 
    "  'num_oper' subject scale or subscale with 5 plausible values. They are:", 
    "    'mrps11' 'mrps12' 'mrps13' 'mrps14' 'mrps15'", 
    "", "  'measurement' subject scale or subscale with 5 plausible values. They are:", 
    "    'mrps21' 'mrps22' 'mrps23' 'mrps24' 'mrps25'", 
    "", "  'geometry' subject scale or subscale with 5 plausible values. They are:", 
    "    'mrps31' 'mrps32' 'mrps33' 'mrps34' 'mrps35'", 
    "", "  'data_anal_prob' subject scale or subscale with 5 plausible values. They are:", 
    "    'mrps41' 'mrps42' 'mrps43' 'mrps44' 'mrps45'", 
    "", "  'algebra' subject scale or subscale with 5 plausible values. They are:", 
    "    'mrps51' 'mrps52' 'mrps53' 'mrps54' 'mrps55'", 
    "", "  'composite' subject scale or subscale with 5 plausible values (the default). They are:", 
    "    'mrpcm1' 'mrpcm2' 'mrpcm3' 'mrpcm4' 'mrpcm5'", "")
  co <- capture.output(showPlausibleValues(sdf,verbose=TRUE))
  expect_equal(co, spv)
  sw <- c("There are 1 full sample weight(s) in this edsurvey.data.frame", 
"  'origwt' with 62 JK replicate weights (the default). Jackknife replicate weight variables:", 
" [1] \"srwt01\" \"srwt02\" \"srwt03\" \"srwt04\" \"srwt05\" \"srwt06\" \"srwt07\" \"srwt08\" \"srwt09\" \"srwt10\" \"srwt11\" \"srwt12\" \"srwt13\" \"srwt14\" \"srwt15\" \"srwt16\" \"srwt17\" \"srwt18\" \"srwt19\" \"srwt20\" \"srwt21\" \"srwt22\" \"srwt23\" \"srwt24\" \"srwt25\" \"srwt26\" \"srwt27\" \"srwt28\" \"srwt29\" \"srwt30\" \"srwt31\" \"srwt32\" \"srwt33\" \"srwt34\" \"srwt35\" \"srwt36\" \"srwt37\" \"srwt38\" \"srwt39\" \"srwt40\" \"srwt41\" \"srwt42\" \"srwt43\" \"srwt44\" \"srwt45\" \"srwt46\" \"srwt47\" \"srwt48\" \"srwt49\" \"srwt50\" \"srwt51\" \"srwt52\" \"srwt53\" \"srwt54\" \"srwt55\"",
"[56] \"srwt56\" \"srwt57\" \"srwt58\" \"srwt59\" \"srwt60\" \"srwt61\" \"srwt62\"",
"")
  co <- capture.output(showWeights(sdf,verbose=TRUE))
  expect_equal(co, sw)
})

context("getData")
test_that("getData", {
  expect_equal_to_reference(gd1 <- getData(sdf, c("dsex", "b017451")), file="gd1.rds")
  skip_on_cran()
  expect_equal_to_reference(gd2 <- getData(sdf, c("dsex", "b017451"), defaultConditions=FALSE), file="gd2.rds")
  expect_equal_to_reference(gd3 <- getData(sdf, c("dsex", "b017451"), dropUnusedLevels=FALSE), file="gd3.rds")
  expect_equal_to_reference(gd4 <- getData(sdf, c("dsex", "b017451"), omittedLevels=TRUE, includeNaLabel=FALSE), file="gd4.rds")
  expect_equal_to_reference(gd5 <- getData(sdf, c("dsex", "b017451"), omittedLevels=TRUE, includeNaLabel=TRUE), file="gd5.rds")
  gd6 <- getData(sdf, formula=composite ~ dsex + b017451)
  gd6 <- gd6[c(1:50,(nrow(gd6)-50):nrow(gd6)),]   # this file was larger. slim down a bit.
  expect_equal_to_reference(gd6, file="gd6.rds")
  expect_equal_to_reference(gd7 <- getData(sdf, c("dsex", "b017451")), file="gd7.rds")
  expect_equal_to_reference(gd8 <- getData(sdf, c("dsex", "c052601"), schoolMergeVarStudent="scrpsu", schoolMergeVarSchool="sscrpsu", dropUnusedLevels=FALSE), file="gd8.rds")
  df2 <- getData(sdf,
    c("dsex", "b017451"),
    recode=list(b017451=list(from=c("Never or hardly ever",
      "Once every few weeks",
      "About once a week"),
    to=c("Infrequently")),
    b017451=list(from=c("2 or 3 times a week",
      "Every day"),
    to=c("Frequently"))
    ))
  expect_equal_to_reference(df2, file="df2.rds")
  df3 <- getData(sdf,
    c("dsex", "b017451"),
    recode=list(b017451=list(from=c(1, 2, 3),
    to=c("Infrequently")),
    b017451=list(from=c(4, 5),
    to=c("Frequently"))
    ))
  expect_equal(df2, df3) # recode by label and numeric agree

  sdf_males <- EdSurvey:::subset(sdf, dsex == "Male", verbose=FALSE)
  expect_equal(dim(sdf_males), c(8905, 302))

  sdf_males <- EdSurvey:::subset(sdf, dsex %in% "Male", verbose=FALSE)
  expect_equal(dim(sdf_males), c(8905, 302))
})

context("lm.sdf")
test_that("lm.sdf",{
  skip_on_cran()
  lm1 <- lm.sdf( ~ dsex + b017451, sdf)
  slm1 <- summary(lm1)
  lm1$formula <- NULL
  lm1$call <- NULL
  lm1_read <- readRDS(file="lm1.rds")
  lm1_read$formula <- NULL
  lm1_read$call <- NULL
  expect_equal(lm1, lm1_read)
  
  skip_on_cran()

  expect_equal_to_reference(lm10 <- lm.sdf(composite ~ dsex + b017451, sdf), file="lm10.rds")
  expect_equal_to_reference(lm1f <- lm.sdf(composite ~ dsex + b017451, sdf, relevels=list(dsex="Female")), "lm1f.rds")
  # test that lfactor levels can be used in relevels argument
  lm1f2 <- lm.sdf(composite ~ dsex + b017451, sdf, jrrIMax=1, relevels=list(dsex=2))
  # calls will not be equal
  lm1f$call <- NULL
  lm1f2$call <- NULL
  expect_equal(lm1f, lm1f2)
  expect_equal(summary(lm1f), summary(lm1f2))
})


context("lm regression testing, Taylor series")
test_that("lm regression testing",{
  skip_on_cran()
  expect_is(sdf_taylor <- lm.sdf(composite ~ sdracem + dsex + pared,
                                 subset(sdf, pared == 1 | pared == 2, verbose=FALSE),
                                 weightVar="origwt",
                                 varMethod = "Taylor",
                                 jrrIMax=Inf),
            "edsurveyLm")
  expect_equal_to_reference(lm1t <- lm.sdf(composite ~ dsex + b017451, sdf, varMethod="Taylor"), "lm1t.rds")
  expect_equal_to_reference(lm1jk <- lm.sdf(composite ~ dsex + b017451, sdf, varMethod="Jackknife"), "lm1.rds")
  # estimates should agree too
  expect_equal(coef(lm1t), coef(lm1jk))
  skip_on_cran()

  lm2ta <- lm.sdf(composite ~ dsex + sdracem + yrsmath, sdf, varMethod="Taylor")
  lm2jka <- lm.sdf(composite ~ dsex + sdracem + yrsmath, sdf, varMethod="Jackknife")
  # check only estimates
  expect_equal(coef(lm2ta), coef(lm2jka))

  expect_equal_to_reference(lm2t <- lm.sdf(composite ~ dsex + sdracem + yrsmath, sdf, varMethod="Taylor", relevel=list(dsex="Female")), "lm2t.rds")
  expect_equal_to_reference(lm2jk <- lm.sdf(composite ~ dsex + sdracem + yrsmath, sdf, varMethod="Jackknife", relevel=list(dsex="Female")), "lm2.rds")
  expect_equal(coef(lm2t), coef(lm2jk))
})

context("edsurveyTable")
test_that("edsurveyTable",{
  skip_on_cran()
  # two levels, results checked vs Primer
  expect_equal_to_reference(es1 <- edsurveyTable(composite ~ dsex + b017451, sdf, jrrIMax=1), file="es1.rds")
  # test no LHS variable
  expect_equal_to_reference(es10 <- edsurveyTable( ~ dsex + b017451, sdf, jrrIMax=1), file="es1.rds")
  # check for just males (dsex is only occupied at one level)
  sdfm <- subset(sdf, dsex=="Male", verbose=FALSE)
  expect_equal_to_reference(es2 <- edsurveyTable(composite ~ dsex + b017451, sdfm, jrrIMax=Inf), file="es2.rds")
  # test unbalanced tables, this check verified. See email from Ting on 10/1/2015 at 5:25 Eastern
  expect_equal_to_reference(es3 <- edsurveyTable(composite ~ lep + ell3 , sdf, jrrIMax=Inf), file="es3.rds")
  # check return.means and return.sepct arguments
  es3b <- edsurveyTable(composite ~ lep + ell3 , sdf, jrrIMax=Inf, returnMeans =  FALSE)
  expect_equal(es3b$data,es3b$data[,c("lep", "ell3", "N", "WTD_N", "PCT", "SE(PCT)")])
  es3c <- edsurveyTable(composite ~ lep + ell3 , sdf, jrrIMax=Inf, returnMeans =  FALSE, returnSepct = FALSE)
  expect_equal(es3c$data, es3$data[,c("lep", "ell3", "N", "WTD_N", "PCT")])
  # test unbalanced tables, with three levels. This check verified. See email from Ting on 10/1/2015 at 5:25 Eastern
  expect_equal_to_reference(es4 <- edsurveyTable(composite ~ lep + ell3 +dsex, sdf, jrrIMax=Inf), file="es4.rds")

  # test omittedLevels
  expect_equal_to_reference(es2b <- edsurveyTable(composite ~ dsex + b017451, sdfm, jrrIMax=Inf, omittedLevels=FALSE), file="es2b.rds")    # this is just a change test, not a confirmed correct test
})

context("edsurveyTable: Taylor")
test_that("edsurveyTable: Taylor",{
  skip_on_cran()
  expect_equal_to_reference(es1t <- edsurveyTable(composite ~ dsex + b017451, sdf, jrrIMax=1, varMethod="Taylor"), file="es1t.rds")
  es1j <- readRDS("es1.rds")
  es1t$njk <- NULL
  es1j$njk <- NULL
  es1t$varMethod <- NULL
  es1j$varMethod <- NULL
  es1j$data["SE(PCT)"] <- NULL
  es1t$data["SE(PCT)"] <- NULL
  es1j$data["SE(MEAN)"] <- NULL
  es1t$data["SE(MEAN)"] <- NULL
  expect_equal(es1j, es1t)
  # check for just males (dsex is only occupied at one level)
  sdfm <- subset(sdf, dsex=="Male", verbose=FALSE)
  expect_equal_to_reference(es2t <- edsurveyTable(composite ~ dsex + b017451, sdfm, jrrIMax=Inf, varMethod="Taylor"), file="es2t.rds")
  # test unbalanced tables
  expect_equal_to_reference(es3t <- edsurveyTable(composite ~ lep + ell3 , sdf, jrrIMax=Inf, varMethod="Taylor"), file="es3t.rds")
  # test unbalanced tables
  expect_equal_to_reference(es4t <- edsurveyTable(composite ~ lep + ell3 +dsex, sdf, jrrIMax=Inf, varMethod="Taylor"), file="es4t.rds")
})

test_that("variable label stored as attributes", {
  skip_on_cran()
  est1 <- edsurveyTable(composite ~ dsex + b017451, sdf, jrrIMax=1)
  expect_equal(attr(est1$data$dsex, "label"), "Gender")
  expect_equal(attr(est1$data$b017451, "label"), "Talk about studies at home")
})

context("showCutPoints")
test_that("showCutPoints",{
  sw <- c("Achievement Levels:", 
          "  Basic:  262", 
          "  Proficient:  299",
          "  Advanced:  333")
  co <- capture.output(showCutPoints(sdf))
  expect_equal(co, sw)
})

context("cor.sdf")
test_that("sdf correlation", {
  skip_on_cran()
  expect_is(expect_c1_pear <- cor.sdf("b017451", "b003501", sdf, method="Pearson", weightVar="origwt"), "edsurveyCor")
  skip_on_cran()
  expect_is(c1_spear <- cor.sdf("b017451", "b003501", sdf, method="Spearman", weightVar="origwt"), "edsurveyCor")
  expect_is(c1_polyc <- cor.sdf("b017451", "b003501", sdf, method="Polychoric", weightVar="origwt"), "edsurveyCor") # takes awhile

  sdf_dnf <- EdSurvey:::subset(sdf, b003601 == 1,verbose=FALSE)
  expect_is(c2_pear <- cor.sdf("composite", "b017451", sdf_dnf, method="Pearson", weightVar="origwt"), "edsurveyCor")
  expect_is(c2_spear <- cor.sdf("composite", "b017451", sdf_dnf, method="Spearman", weightVar="origwt"), "edsurveyCor")
  expect_is(c2_polys <- cor.sdf("composite", "b017451", sdf_dnf, method="Polyserial", weightVar="origwt"), "edsurveyCor")
})

context("cor.sdf errors")
test_that("In cor, variables as class", {
  skip_on_cran()
  df=getData(sdf, c("b017451", "sdracem","origwt"),addAttributes = TRUE)
  df$sdracem <- as.character(df$sdracem)
  expect_error(cor.sdf("b017451", "sdracem",df,method="Pearson"))
})

context("cor.sdf reorder")
test_that("Reordering a variable manually vs through cor.sdf", {
  skip_on_cran()
  gddat=getData(sdf, c("b017451", "sdracem","origwt"),addAttributes = TRUE)
  gddat$sdracem <- factor(gddat$sdracem,levels=c("White","Hispanic","Black","Asian/Pacific Island","Amer Ind/Alaska Natv","Other"))
  cor3 <- cor.sdf("b017451","sdracem", sdf, method="Pearson", weightVar="origwt", reorder=list(sdracem=c("White","Hispanic","Black","Asian/Pacific Island","Amer Ind/Alaska Natv","Other")))
  cor4 <- cor.sdf("b017451","sdracem", gddat, method="Pearson", weightVar="origwt")
  expect_equal(cor3,cor4)

  gddat$sdracem[gddat$sdracem=="Hispanic"] <- "White"
  gddat$sdracem <- factor(gddat$sdracem,levels=c("White", "Black","Asian/Pacific Island", "Amer Ind/Alaska Natv", "Other"))
  cor1Pe <- cor.sdf("b017451","sdracem", sdf, method="Pearson", weightVar="origwt",recode = list(sdracem=list(from="Hispanic", to="White")))
  cor2Pe <- cor.sdf("b017451","sdracem", gddat, method="Pearson", weightVar="origwt")
  expect_equal(cor1Pe,cor2Pe)

  cor1Sp <- cor.sdf("b017451","sdracem", sdf, method="Spearman", weightVar="origwt",recode = list(sdracem=list(from="Hispanic", to="White")))
  cor2Sp <- cor.sdf("b017451","sdracem", gddat, method="Spearman", weightVar="origwt")
  expect_equal(cor1Sp,cor2Sp)

  cor1pc <- cor.sdf("b017451","sdracem", sdf, method="Polychoric", weightVar="origwt",recode = list(sdracem=list(from="Hispanic", to="White")))
  cor2pc <- cor.sdf("b017451","sdracem", gddat, method="Polychoric", weightVar="origwt")
  expect_equal(cor1pc,cor2pc)

  sdf_dnf <- EdSurvey:::subset(sdf, sdracem==5 | sdracem==3 | sdracem==1,verbose=FALSE)
  cc1_pear <- cor.sdf("b017451", "sdracem", sdf_dnf, method="Pearson", weightVar="origwt", recode = list(sdracem=list(from=3, to=1)))
  cc1_spear <- cor.sdf("b017451", "sdracem", sdf_dnf, method="Spearman", weightVar="origwt", recode = list(sdracem=list(from=3, to=1)))
  # the polyc tests are slow but have caught an error in the past
  cc1_polyc <- cor.sdf("b017451", "sdracem", sdf_dnf, method="Polychoric", weightVar="origwt", recode = list(sdracem=list(from=3, to=1)))
  cc2_pear <- cor.sdf("b017451", "sdracem", sdf_dnf, method="Pearson", weightVar="origwt", recode = list(sdracem=list(from="Hispanic", to="White")))
  cc2_spear <- cor.sdf("b017451", "sdracem", sdf_dnf, method="Spearman", weightVar="origwt", recode = list(sdracem=list(from="Hispanic", to="White")))
  cc2_polyc <- cor.sdf("b017451", "sdracem", sdf_dnf, method="Polychoric", weightVar="origwt", recode = list(sdracem=list(from="Hispanic", to="White")))
  expect_equal(cc1_pear,cc2_pear)
  expect_equal(cc1_spear,cc2_spear)
  expect_equal(cc1_polyc,cc2_polyc)
})

test_that("unweighted cor works", {
  skip_on_cran()
  # in some ways this is maybe more of a test of getData
  b1a <- cor.sdf("m815401", "b017451",method="Pearson", sdf,weightVar = "origwt")
  b1b <- cor.sdf("m815401", "b017451",method="Pearson", sdf,weightVar = NULL)
  expect_equal(b1a$correlation,b1b$correlation, tolerance=0.02, scale=1)

  b2a <- cor.sdf("m815401", "b017451",method="Spearman", sdf,weightVar = "origwt")
  b2b <- cor.sdf("m815401", "b017451",method="Spearman", sdf,weightVar = NULL)
  expect_equal(b2a$correlation,b2b$correlation, tolerance=0.02, scale=1)
} )

context("achievementLevels")
test_that("achievementLevels basic", {
  skip_on_cran()
  expect_equal_to_reference(test1 <- achievementLevels(returnCumulative = TRUE, data=sdf), file="aLevels_test1.rds")
})

test_that("achievementLevels, aggregated by non PV variables", {
  skip_on_cran()
  expect_equal_to_reference(test2 <- achievementLevels(aggregateBy = "dsex",returnCumulative = TRUE, data=sdf), file="aLevels_test2.rds")
  expect_equal_to_reference(test3 <- achievementLevels(aggregateBy = "sdracem",returnCumulative = TRUE, data=sdf), file="aLevels_test3.rds")
})

test_that("achievementLevels, aggregated by composite", {
  skip_on_cran()
  expect_equal_to_reference(test4 <- achievementLevels("sdracem",aggregateBy = c("composite"),data=sdf, returnCumulative = TRUE), file="aLevels_test4.rds")
  expect_equal_to_reference(test5 <- achievementLevels("dsex",aggregateBy = c("composite"),data=sdf, returnCumulative = TRUE), file="aLevels_test5.rds")
  # Use recode to change values for specified variables:
  expect_equal_to_reference(test6 <- achievementLevels(c("composite","dsex", "b017451"),
                                                       aggregateBy = "dsex", sdf,
                                                       recode=list(b017451=list(from=c("Never or hardly ever",
                                                                                       "Once every few weeks",
                                                                                       "About once a week"),
                                                                                to=c("Infrequently")),
                                                                   b017451=list(from=c("2 or 3 times a week","Every day"),
                                                                                to=c("Frequently")))), file="aLevels_test6.rds")
})

test_that("achievementLevels complex", {
  skip_on_cran()
  expect_equal_to_reference(test7 <- achievementLevels(c("composite", "ell3", "lep", "pared", "b017451"), data=sdf, returnCumulative = TRUE), file="aLevels_test7.rds")
})