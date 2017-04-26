# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# table to compare to subsequent tables with subsets:
edsurveyTable(~ dsex, sdf, returnMeans=FALSE, returnSepct=FALSE)

# subset to just males
newsdf <-  subset(sdf, dsex == "Male") 
# table of dsex after subset
edsurveyTable(~ dsex, newsdf, returnMeans=FALSE, returnSepct=FALSE)

# Variable names that are not in the sdf get resolved in the parent frame.
# Practically, that means that the following two subset 
# calls sdfM1 and sdfM2 do the same thing:
male_var <- "Male"
sdfM1 <- subset(sdf,dsex == male_var)
sdfM2 <- subset(sdf,dsex == "Male")
table(getData(sdfM1, "dsex"))
table(getData(sdfM2, "dsex"))

# variable can also be resolved as members of lists
genders <- c("Male", "Female","not a sex level")
sdfn <- subset(sdf,dsex == genders[2]) 
table(getData(sdfn, "dsex"))

# variables can also be subset using %in%
sdfM3 <- subset(sdf,dsex %in% c("Male", "not a sex level")) 
table(getData(sdfM3, "dsex"))

# if you need to call a name on the sdf dynamically, you can use as.name
dsex_standin <- as.name("dsex")
sdfM4 <- subset(sdf,dsex_standin == "Male")
table(getData(sdfM4, "dsex"))


# Here is an example of how one might want to call
# subset from within a function or loop.
# First,define a few variables to use dynamically
rhs_vars <- c("dsex", "b017451")
lvls <- c("Male", "Female")

# create a parsed condition
cond <- parse(text=paste0(rhs_vars[1], " == \"",lvls[1],"\""))[[1]]

# when inside=TRUE a parsed condition can be passed to subset
dsdf <- subset(sdf, cond, inside=TRUE)

# check the result
table(getData(dsdf, "dsex"))
