\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package="NAEPprimer"))

# Discrete achievement Levels
achievementLevels(achievementVars=c("composite"), aggregateBy=NULL, data=sdf) 

# Cumulative achievement Levels
achievementLevels(achievementVars=c("composite"), aggregateBy=NULL, data=sdf, 
                  returnCumulative=TRUE) 

# Achievement levels as independent variables, by sex aggregated by composite
achievementLevels(achievementVars=c("composite", "dsex"), aggregateBy="composite",
                  data=sdf, returnCumulative=TRUE) 

# Achievement levels as independent variables, by sex aggregated by sex
achievementLevels(achievementVars=c("composite", "dsex"), aggregateBy="dsex", 
                  data=sdf, returnCumulative=TRUE) 

# Achievement levels as independent variables, by race aggregated by race
achievementLevels(achievementVars=c("composite", "sdracem"),
                  aggregateBy="sdracem", data=sdf, returnCumulative=TRUE) 

# Use recode to change values for specified variables:
achievementLevels(achievementVars=c("composite","dsex", "b017451"),
                  aggregateBy = "dsex", sdf,
                  recode=list(b017451=list(from=c("Never or hardly ever",
                                                  "Once every few weeks",
                                                  "About once a week"),
                                           to="Infrequently"),
                              b017451=list(from=c("2 or 3 times a week",
                                                  "Every day"),
                                           to="Frequently")))

}
