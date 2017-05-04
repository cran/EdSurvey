## ----readNAEP library, echo=FALSE----------------------------------------
library(EdSurvey)

## ----readNAEP vignette, eval=FALSE---------------------------------------
#  library(EdSurvey)
#  sdf <- readNAEP(filepath='//.../Data/file.dat')

## ----readNAEP source package---------------------------------------------
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

## ----read in the data----------------------------------------------------
gddat <- getData(data=sdf, varnames=c('composite', 'dsex', 'b017451', 'origwt'),
                addAttributes=TRUE, omittedLevels=FALSE)

## ----getData-------------------------------------------------------------
gddat <- getData(data=sdf, formula=composite ~ dsex + b017451, varnames="origwt", 
                 addAttributes=TRUE, defaultConditions=TRUE)

## ----head(gddat)---------------------------------------------------------
head(x=gddat[,1:7])

## ----schoolmerge---------------------------------------------------------
gddat <- getData(data=sdf, varnames=c("composite", "dsex", "b017451","c052601","origwt"),
                 schoolMergeVarStudent='scrpsu', schoolMergeVarSchool="sscrpsu",
                 addAttributes=TRUE)

## ----head(gddat) schoolmerge---------------------------------------------
head(x=gddat[,1:7])

## ----Base Recode Column--------------------------------------------------
# 1. Recode a Column Based on a String

gddat$b017451 <- gsub(pattern="Every day",replacement="Seven days a week",x=gddat$b017451)
head(x=gddat$b017451)

## ----Base Subset Data with Labels----------------------------------------
# 2. Subset the Data Based on a String

df <- subset(gddat,b017451 == "2 or 3 times a week" | b017451 == "About once a week")
head(x=df[,1:7])

## ----Base Subset Data with Levels----------------------------------------
# 2. Subset the Data Based on a String
gddat <- getData(data=sdf, varnames=c("composite", "dsex", "b017451", "c052601", "origwt"),
                schoolMergeVarStudent='scrpsu', schoolMergeVarSchool="sscrpsu",
                addAttributes=TRUE)

df <- subset(gddat, b017451 == 4 | b017451 == 3)
head(x=df[,1:7])

## ----edsurveyTable gddat-------------------------------------------------
es2 <- edsurveyTable(composite ~ dsex + b017451, weightVar="origwt", data=gddat)

## ----knitr edsurveyTable gddat, echo=FALSE-------------------------------
knitr::kable(x=es2$data, digits=7, row.names=FALSE, caption="Table es2")

## ----lm.sdf gddat--------------------------------------------------------
lm2 <- lm.sdf(composite ~ dsex + b017451, weightVar="origwt", data=gddat)
summary(lm2)

## ----lm.sdf3 gddat-------------------------------------------------------
lm3 <- lm.sdf(composite ~ dsex + b017451, data=gddat, relevels=list(dsex="Female"))
summary(lm3)

## ----cor.sdf gddat-------------------------------------------------------
eddat <- getData(data=sdf, varnames=c("num_oper","algebra","dsex", 'origwt'),
                addAttributes=TRUE, omittedLevels=FALSE)

eddat <- subset(eddat,dsex == "Female")
cor2 <- cor.sdf(x="num_oper",y="algebra", weightVar="origwt", data=eddat, method="Pearson")
cor2

## ----sample getData------------------------------------------------------
rsdf <- getData(data=sdf, varnames=c(all.vars(composite ~ sdracem + iep),"origwt"), 
                addAttributes=TRUE)

## ----sample recode-------------------------------------------------------
rsdf$sdracem <-gsub(pattern="Amer Ind/Alaska Natv|Other",replacement="Other",x=rsdf$sdracem)
unique(x=rsdf$sdracem)

## ----sample regression---------------------------------------------------
lm4 <- lm.sdf(composite ~ iep + sdracem, weightVar="origwt", data=rsdf)
summary(lm4)

## ----sample getData full-------------------------------------------------
eddat <- getData(data=rsdf, varnames=c(all.vars(composite ~ sdracem + iep),"origwt"),
                 recode=list(sdracem=list(from=c("Amer Ind/Alaska Natv|Other"),
                                          to=c("Other"))),
                 addAttributes=TRUE)

## ----sample regression2--------------------------------------------------
lm5 <- lm.sdf(composite ~ iep + sdracem, weightVar="origwt", data=eddat)
summary(lm5)

## ----sample2 getData-----------------------------------------------------
gddat <- getData(data=sdf, varnames=c(all.vars(composite ~ lep + dsex + iep),"origwt"), 
                 addAttributes=TRUE, omittedLevels=FALSE)

## ----sample2 unique subset-----------------------------------------------
unique(x=gddat[,c("lep","dsex","iep")])

## ----sample2 unique subset2----------------------------------------------
gddat=subset(gddat,iep %in% c("No", "Yes"))
gddat=subset(gddat,lep %in% c("No", "Yes"))
unique(x=gddat[,c("lep","dsex","iep")])

## ----sample2 recode------------------------------------------------------
lm6 <- lm.sdf(composite ~ lep + dsex + iep, weightVar="origwt", gddat)
summary(lm6)

## ----sample3 getData-----------------------------------------------------
comp <- getData(data=sdf, varnames=c("composite", "t088801", "t088803", "t088804", "t088805","origwt"), 
                 addAttributes=TRUE)

## ----sample3 recode------------------------------------------------------
comp_vars <- c("t088801", "t088803", "t088804", "t088805")
comp[,comp_vars] <- sapply(X=comp[,comp_vars], FUN=as.numeric)
comp$t08880a <- comp$t088801 + comp$t088803 + comp$t088804 + comp$t088805
names(comp)

## ----sample3 lm----------------------------------------------------------
comp_lm <- lm.sdf(composite ~ t08880a, weightVar="origwt", data=comp)
summary(comp_lm)

## ----mem_usage gddat-----------------------------------------------------
object.size(gddat <- getData(data=sdf, varnames=c('composite', 'dsex', 'b017451', 'origwt'),
                             addAttributes=TRUE, omittedLevels=FALSE))
object.size(lm7 <- lm.sdf(composite ~ dsex + b017451,
                          weightVar='origwt', data=gddat))
object.size(lm8 <- lm.sdf(composite ~ dsex + b017451,
                          weightVar='origwt', data=sdf))

## ----rm df---------------------------------------------------------------
rm(df,gddat,eddat,rsdf)

## ----eval=FALSE----------------------------------------------------------
#  gc()

## ----eval=FALSE----------------------------------------------------------
#  gddat <- getData(data=sdf, varnames=c(all.vars(composite ~ lep + dsex + iep),"origwt"),
#                   addAttributes=TRUE, omittedLevels=FALSE)
#  lm9 <- lm.sdf(composite ~ lep + dsex + iep + b017451, data=gddat)
#  
#  ## Using default weight variable 'origwt'
#  
#  ## Error in getData(sdf, c(all.vars(formula), wgt), ..., includeNaLabel=TRUE)
#    ## The following variable names are required for this call
#    ## and are not on the incoming data 'b017451'.

## ----lm forgot column----------------------------------------------------
gddat <- getData(data=sdf, varnames=c(all.vars(composite ~ lep + dsex + iep + b017451),"origwt"), 
                 addAttributes=TRUE, omittedLevels=FALSE)
lm9 <- lm.sdf(composite ~ lep + dsex + iep + b017451, data=gddat)
lm9

