edsurvey.data.frame <- function(userConditions, 
                                defaultConditions, 
                                data, 
                                dataSch, 
                                weights, 
                                pvvars, 
                                subject, 
                                year, 
                                assessmentCode, 
                                dataType, 
                                gradeLevel, 
                                achievementLevels, 
                                omittedLevels, 
                                fileFormat, 
                                fileFormatSchool, 
                                survey,
                                country,
                                psuVar,
                                stratumVar,
                                jkSumMultiplier) {
  achievementLevels <- sort(achievementLevels)
  if(min(nchar(names(achievementLevels)))==0) {
    stop(paste0("The argument ", sQuote("achievementLevels"), " must have nonmissing names for each level."))
  }

  res <- list(userConditions=userConditions, 
              defaultConditions=defaultConditions, 
              data=data, 
              dataSch=dataSch, 
              weights=weights, 
              pvvars=pvvars, 
              subject=subject, 
              year=year, 
              assessmentCode=assessmentCode, 
              dataType=dataType, 
              gradeLevel=gradeLevel, 
              achievementLevels=achievementLevels, 
              omittedLevels=omittedLevels, 
              fileFormat=fileFormat, 
              fileFormatSchool=fileFormatSchool, 
              survey=survey,
              country=country,
              psuVar=psuVar,
              stratumVar=stratumVar,
              jkSumMultiplier=jkSumMultiplier,
              dim0=c(nrow(data[,1]),
                     length(c(names(data), names(dataSch)))))
  class(res) <- "edsurvey.data.frame"
  res
}
