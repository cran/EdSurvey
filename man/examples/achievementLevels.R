\dontrun{
  # read in the example data (generated, not real student data)
  sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

  # Discrete achievement Levels
  achievementLevels(c("composite"), aggregateBy = NULL, sdf) 

  # Cumulative achievement Levels
  achievementLevels(c("composite"), aggregateBy = NULL, sdf, 
                             returnCumulative = TRUE) 

  # Achievement levels as independent variables, by sex aggregated by composite
  achievementLevels(c("composite", "dsex"), aggregateBy = "composite",
                             sdf, returnCumulative = TRUE) 

  # Achievement levels as independent variables, by sex aggregated by sex
  achievementLevels(c("composite", "dsex"), aggregateBy = "dsex", 
                             sdf, returnCumulative = TRUE) 

  # Achievement levels as independent variables, by race aggregated by race
  achievementLevels(c("composite", "sdracem"),
                             aggregateBy = "sdracem", sdf, returnCumulative = TRUE) 

  # Use recode to change values for specified variables:
  achievementLevels(c("composite","dsex", "b017451"),
                             aggregateBy = "dsex", sdf,
                             recode=list(
                               b017451=list(
                                 from=c("Never or hardly ever",
                                        "Once every few weeks","About once a week"),
                                 to=c("Infrequently")),
                               b017451=list(
                                 from=c("2 or 3 times a week","Every day"),
                                 to=c("Frequently"))))

  # use just one achievement level
  sdfA <- sdf
  sdfA$achievementLevels <- sdfA$achievementLevels["Proficient"]
  achievementLevels(c("composite"), aggregateBy = NULL, sdfA) 

}
