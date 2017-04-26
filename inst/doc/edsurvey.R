## ----input code----------------------------------------------------------
inputCode <- c(2,"neat")

## ----input output--------------------------------------------------------
inputCode

## ----code options, echo = FALSE----------------------------------------------------
options(width = 85)

## ----source package, eval=FALSE----------------------------------------------------
#  install.packages("EdSurvey")

## ----load package------------------------------------------------------------------
library(EdSurvey)

## ----readNAEP, source package------------------------------------------------------
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

## ----readNAEPvig, eval=FALSE-------------------------------------------------------
#  sdf2 <- readNAEP('//.../Data/file.dat')

## ----print SDF---------------------------------------------------------------------
sdf

## ----data.frame like, warning=FALSE------------------------------------------------
dim(sdf)
nrow(sdf)
ncol(sdf)

## ----names-------------------------------------------------------------------------
names(sdf)

## ----searchSDF---------------------------------------------------------------------
searchSDF("book", sdf, fileFormat="student")

## ----searchSDF levels--------------------------------------------------------------
searchSDF("book", sdf, fileFormat="student", levels = TRUE)

## ----levelsSDF levels--------------------------------------------------------------
levelsSDF("b017451", sdf)

## ----pv and weights----------------------------------------------------------------
showPlausibleValues(sdf, verbose=TRUE)
showWeights(sdf, verbose=TRUE)

## ----edsurveyTable, cache=FALSE, warning=FALSE-------------------------------------
es1 <- edsurveyTable(composite ~ dsex + b017451, sdf, 
                     jrrIMax=1, varMethod="jackknife")

## ---- eval=FALSE-------------------------------------------------------------------
#  es1$data

## ----edsurveyTable kable, results='asis', echo=FALSE-------------------------------
knitr::kable(es1$data, digits=7, row.names=FALSE, caption="es1")

## ----edsurveyTable taylor, cache=FALSE, warning=FALSE------------------------------
es1t <- edsurveyTable(composite ~ dsex + b017451, sdf, 
                     jrrIMax=1, varMethod="Taylor")

## ---- eval=FALSE-------------------------------------------------------------------
#  es1t$data

## ----edsurveyTable Taylor kable, results='asis', echo=FALSE------------------------
knitr::kable(es1t$data, digits=7, row.names=FALSE, caption="es1t")

## ----edsurveyTable1b, cache=FALSE, warning=FALSE-----------------------------------
es1b <- edsurveyTable(composite ~ dsex + b017451, sdf, jrrIMax=1,
                      returnMeans=FALSE, returnSepct=FALSE)

## ---- eval=FALSE-------------------------------------------------------------------
#  es1b

## ----edsurveyTable1b kable, results='asis', echo=FALSE-----------------------------
knitr::kable(es1b$data, digits=7, row.names=FALSE, caption="es1b")

## ---- eval=FALSE-------------------------------------------------------------------
#  ?edsurveyTable

## ----showCutPoints-----------------------------------------------------------------
showCutPoints(sdf)

## ----overall achievement levels----------------------------------------------------
aLev0 <- achievementLevels(achievementVars=c("composite"),
                           data=sdf, returnCumulative=TRUE)

## ---- eval=FALSE-------------------------------------------------------------------
#  aLev0$discrete

## ----aL0 kable, results='asis', echo=FALSE-----------------------------------------
knitr::kable(aLev0$discrete, digits=7, row.names=FALSE, caption="aLev0$discrete")

## ----achievementLevels dsex--------------------------------------------------------
aLev1 <- achievementLevels(c("composite", "dsex"), aggregateBy="dsex",
                  data=sdf, returnCumulative=TRUE)

## ---- eval=FALSE-------------------------------------------------------------------
#  aLev1$discrete

## ----aL1 kable, results='asis', echo=FALSE-----------------------------------------
knitr::kable(aLev1$discrete, digits=7, row.names=FALSE, caption="aLev1$discrete")

## ---- eval=FALSE-------------------------------------------------------------------
#  aLev1$cumulative

## ----aL1 kable 2, results='asis', echo=FALSE---------------------------------------
knitr::kable(aLev1$cumulative, digits=7, row.names=FALSE, caption="aLev1$cumulative")

## ----achievementLevels2------------------------------------------------------------
achievementLevels(c("composite", "dsex", "iep"), aggregateBy=c("dsex", "iep"),
                  data=sdf)
achievementLevels(c("composite", "dsex", "iep"), aggregateBy=c("iep", "dsex"),
                  data=sdf)

## ----aLev2 characteristics, cache=FALSE, warning=FALSE-----------------------------
aLev2 <- achievementLevels(c("composite", "dsex"), aggregateBy="composite",
                           data=sdf, returnCumulative=TRUE)
aLev2$discrete
aLev2$cumulative

## ----aLev3 three-way, cache=FALSE, warning=FALSE-----------------------------------
aLev3 <- achievementLevels(c("composite", "dsex", "lep"),
                           aggregateBy=c("dsex", "composite"),
                           data=sdf,
                           returnCumulative=TRUE)
aLev3$discrete
aLev3$cumulative

## ----aLev4 cutpoints, cache=FALSE, warning=FALSE-----------------------------------
aLev4 <- achievementLevels(c("composite", "dsex"),
                           aggregateBy="dsex",
                           data=sdf,
                           cutpoints=c(267, 299, 333),
                           returnCumulative=TRUE)

aLev4$discrete
aLev1$discrete

## ----lm, cache=FALSE, warning=FALSE------------------------------------------------
lm1 <- lm.sdf(composite ~ dsex + b017451, sdf)
summary(lm1)

## ----lmf, cache=FALSE, warning=FALSE-----------------------------------------------
lm1f <- lm.sdf(composite ~ dsex + b017451, sdf,
               relevels=list(dsex="Female"))
summary(lm1f)

## ----cor pearson, cache=FALSE, warning=FALSE---------------------------------------
cor_pearson <- cor.sdf("b013801", "t088001", sdf, 
                    method="Pearson", weightVar="origwt")

## ----cor, warning=FALSE------------------------------------------------------------
cor_pearson

## ----cor recode, cache=FALSE, warning=FALSE----------------------------------------
cor_recode <- cor.sdf("b017451","t088001", sdf, 
                      method="Pearson", weightVar="origwt",
                      recode=list(b017451=list(from=c("2 or 3 times a week", "Every day"),
                                               to=c("Frequently"))))
cor_recode

## ----cor reorder, cache=FALSE, warning=FALSE---------------------------------------
cor_reorder <- cor.sdf("b017451","t088001", sdf, 
                       method="Pearson", weightVar="origwt",
                       reorder=list(t088001=c("7 hours or more","5-6.9 hours",
                                              "3-4.9 hours","Less than 3 hours")))
cor_reorder

## ----cor marginal, cache=FALSE, warning=FALSE--------------------------------------
cor3_mcc <- cor.sdf("num_oper", "algebra", sdf, method="Pearson")
cor3_mcc

## ----cor continuous, cache=FALSE, warning=FALSE------------------------------------
sdf_dnf <- subset(sdf, dsex == 1)
cor_pearson <- cor.sdf("b017451", "pared", sdf_dnf, 
                    method="Pearson", weightVar="origwt")
cor_spearman <- cor.sdf("b017451", "pared", sdf_dnf, 
                    method="Spearman", weightVar="origwt")
cor_polychoric <- cor.sdf("b017451", "pared", sdf_dnf, 
                    method="Polychoric", weightVar="origwt")

## ----cor comparison, warning=FALSE-------------------------------------------------
cbind(Correlation=c(Pearson=cor_pearson$correlation,
                    Spearman=cor_spearman$correlation,
                    Polychoric=cor_polychoric$correlation))

## ----cor categorical, cache=FALSE, warning=FALSE-----------------------------------
cor_pearson2 <- cor.sdf("composite", "b017451", sdf_dnf, 
                    method="Pearson", weightVar="origwt")
cor_spearman2 <- cor.sdf("composite", "b017451", sdf_dnf, 
                    method="Spearman", weightVar="origwt")
cor_polyserial2 <- cor.sdf("composite", "b017451", sdf_dnf, 
                    method="Polyserial", weightVar="origwt")

## ----cor comparison2, warning=FALSE------------------------------------------------
cbind(Correlation=c(Pearson=cor_pearson2$correlation,
                    Spearman=cor_spearman2$correlation,
                    Polyserial=cor_polyserial2$correlation))

## ----correlation unweighted, cache=FALSE, warning=FALSE----------------------------
cor_pearson_unweighted <- cor.sdf("b017451", "pared", sdf, 
                    method="Pearson", weightVar=NULL)
cor_pearson_unweighted
cor_spearman_unweighted <- cor.sdf("b017451", "pared", sdf, 
                    method="Spearman", weightVar=NULL)
cor_spearman_unweighted

## ----subset, cache=FALSE, warning=FALSE--------------------------------------------
sdfm <- subset(sdf, dsex=="Male" & (sdracem==3 | sdracem==1))
es2 <- edsurveyTable(composite ~ dsex + sdracem, sdfm)

## ---- eval=FALSE-------------------------------------------------------------------
#  es2

## ----subset table, cache=FALSE, results='asis', echo=FALSE-------------------------
knitr::kable(es2$data, digits=7, row.names=FALSE, caption="es2")

## ----getData, cache=FALSE, warning=FALSE-------------------------------------------
gddat <- getData(sdf, varnames=c("dsex","b017451"), omittedLevels=TRUE)
head(gddat)

## ----getData school merge, cache=FALSE, warning=FALSE------------------------------
gddat2 <- getData(sdf, varnames=c("dsex","b017451","c052601"), 
                  schoolMergeVarStudent="scrpsu", schoolMergeVarSchool="sscrpsu")
head(gddat2)

## ----get all data, warning=FALSE---------------------------------------------------
lsdf0 <- getData(sdf, varnames=names(sdf), addAttributes=TRUE,
                 omittedLevels=FALSE, defaultConditions=FALSE)
dim(lsdf0) # excludes the one school variable in the sdf
dim(sdf)

## ----rhelp, eval=FALSE-------------------------------------------------------------
#  help(package="EdSurvey")

