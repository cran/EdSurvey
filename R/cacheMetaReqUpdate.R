#function to determine if/when a cacheMetaFile requires it to reprocessed due to code changes or other modifications
#cacheFileMetaVer is a single value to indicate the .meta file version of the .meta file you wish to test
#returns a logical value.  
#TRUE == The cache .meta file requires to be updated and requires reprocessing
#FALSE == The cache .meta file does not require updating and reprocessing
#' @importFrom utils packageVersion
cacheMetaReqUpdate <- function(cacheFileMetaVer, surveyName=NULL){
  #if the cacheFileMetaVer is null it means the .meta file was build before we implemented this version
  if(is.null(cacheFileMetaVer)){
    return(TRUE)
  }
  
  #unlist the value of 'packageVersion'as it returns a list of integers
  cacheFileMetaVer <- as.numeric(unlist(cacheFileMetaVer)) #the version number of the .meta file stored for the survey when the .meta file was created
  currentPkgVer <- unlist(packageVersion("EdSurvey")) #the current version of the installed EdSurvey package
  
  if(length(cacheFileMetaVer)==0){ #no cacheFileMetaVer specified
    return(TRUE)
  }
  
  surveyDef <- c("NAEP", #NAEP Dataset
               "TIMSS", "TIMSS Advanced", "PIRLS", "ICILS", "ICCS", "CivED", #IEA Datasets
               "PIAAC", "PISA", "TALIS") #OECD Datasets
  
  #build our lookup table
  surveyLookup <- data.frame(survey=surveyDef, cacheVer=vector("integer", length(surveyDef)), stringsAsFactors = FALSE)
  
  #specify the cacheFileVersion here that is the most up to date version for that specific survey type
  surveyLookup[surveyLookup$survey=="NAEP", "cacheVer"] <- 2
  surveyLookup[surveyLookup$survey=="TIMSS", "cacheVer"] <- 3
  surveyLookup[surveyLookup$survey=="TIMSS Advanced", "cacheVer"] <- 2
  surveyLookup[surveyLookup$survey=="PIRLS", "cacheVer"] <- 3
  surveyLookup[surveyLookup$survey=="ICILS", "cacheVer"] <- 2
  surveyLookup[surveyLookup$survey=="ICCS", "cacheVer"] <- 3
  surveyLookup[surveyLookup$survey=="CivED", "cacheVer"] <- 5
  surveyLookup[surveyLookup$survey=="PIAAC", "cacheVer"] <- 3
  surveyLookup[surveyLookup$survey=="PISA", "cacheVer"] <- 4
  surveyLookup[surveyLookup$survey=="TALIS", "cacheVer"] <- 4
  
  if(!any(surveyLookup$survey %in% surveyName)){
    warning("Survey name not recognized while checking .meta version. Forcing cache .meta file to be updated.")
    return(TRUE) 
  }
  
  testVal <- surveyLookup[surveyLookup$survey==surveyName, "cacheVer"]
  
  if(cacheFileMetaVer < testVal){ #test the .meta cache version vs the versin specified
    cacheMetaReqUpdate <- TRUE
  } else {
    cacheMetaReqUpdate <- FALSE
  }
  
  return(cacheMetaReqUpdate)
}
