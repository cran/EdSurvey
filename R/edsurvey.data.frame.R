# build and edsurvey.data.frame from all of the requisite parts.
# There is no need for an S3 object to have a constructor, but having one
# can help programmers (and maintainers) know that every edsurvey.data.frame 
# has all the correct elements.
# @author Tom Fink and Paul Bailey
edsurvey.data.frame <- function(userConditions, 
                                defaultConditions, 
                                data, 
                                dataSch,
                                dataTch,
                                dataListMeta,
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
                                fileFormatTeacher,
                                survey,
                                country,
                                psuVar,
                                stratumVar,
                                jkSumMultiplier,
                                recodes=NULL) {
  if (is.list(achievementLevels)) { 
    achievementLevels <- lapply(achievementLevels, function(l) { 
      sort(l)})
  } else {
    # In the international assessment TALIS, there is no achievement level, account for that here
    if(!is.null(achievementLevels)) { 
      achievementLevels <- sort(achievementLevels) 
      # names can't be missing, but they can be length 0. Figure out if that is the case.
      if(min(nchar(names(achievementLevels)))==0) {
        stop(paste0("The argument ", sQuote("achievementLevels"), " must have nonmissing names for each level."))
      }
      for (pvi in 1:length(pvvars)) {
        # setup achievement levels for all plausible value variables
        temp = list(varnames = pvvars[[pvi]]$varnames)
        temp$achievementLevel <- achievementLevels
        pvvars[[names(pvvars)[pvi]]] <- temp
      }
    }
    
  } #closes else statment following if (is.list(achievementLevels))
  
  #determine full dataset size
  if(!is.null(dataTch)){
    dim0 <- c(nrow(dataTch[,1]),
              length(unique(c(names(data), names(dataSch), names(dataTch)))))
  }else{
    dim0 <- c(nrow(data[,1]),
              length(unique(c(names(data), names(dataSch), names(dataTch)))))
  }
  
  res <- list(userConditions=userConditions, 
              defaultConditions=defaultConditions, 
              data=data, 
              dataSch=dataSch, 
              dataTch=dataTch,
              dataListMeta=dataListMeta,
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
              fileFormatTeacher=fileFormatTeacher,
              survey=survey,
              country=country,
              psuVar=psuVar,
              stratumVar=stratumVar,
              jkSumMultiplier=jkSumMultiplier,
              recodes=recodes,
              dim0=dim0)
  class(res) <- "edsurvey.data.frame"
  
  # we discovered that not closing a LaF connections leads to memory leaks.
  # so, close all of the connections when it is complete (constructed).
  if(!is.null(data)){LaF::close(data)}
  if(!is.null(dataSch)){LaF::close(dataSch)}
  if(!is.null(dataTch)){LaF::close(dataTch)}
  
  return(res)
}
