\dontrun{
# read in the example data (generated, not real student data)

sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# create a table that shows only the break down of dsex
edsurveyTable( ~ dsex, sdf, returnMeans=FALSE, returnSepct=FALSE)

# create a table with composite scores by dsex
edsurveyTable(composite ~ dsex, sdf)

# add a second variable
edsurveyTable(composite ~ dsex + b017451, sdf)

# add a second variable, do not omit any levels
edsurveyTable( ~ dsex + b017451 + b003501, sdf, omittedLevels=FALSE)

# add a second variable, do not omit any levels, change aggregation level
edsurveyTable( ~ dsex + b017451 + b003501, sdf,
                      omittedLevels=FALSE, pctAggregationLevel=0)

edsurveyTable( ~ dsex + b017451 + b003501, sdf,
                      omittedLevels=FALSE, pctAggregationLevel=1)

edsurveyTable( ~ dsex + b017451 + b003501, sdf,
                      omittedLevels=FALSE, pctAggregationLevel=2)

# variance estimation using the Taylor series 
edsurveyTable( ~ dsex + b017451 + b003501, sdf,
                     varMethod="Taylor")
}
