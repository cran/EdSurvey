checkDataClass <- function(data, classes) {
  if(! inherits(data, classes) ) {
    if(sort(classes) == sort(c("edsurvey.data.frame", "light.edsurvey.data.frame"))){
      stop(paste0("The argument ", sQuote("data"), " must be an edsurvey.data.frame or a light.edsurvey.data.frame. See ",
        dQuote(paste0("Using the ", sQuote("EdSurvey"),
        " Package's getData Function to Manipulate the NAEP Primer Data vignette")),
         " for how to work with data in a light.edsurvey.data.frame."))
    }
    if(sort(classes) == sort(c("edsurvey.data.frame", "light.edsurvey.data.frame", "edsurvey.data.frame.list"))){
      stop(paste0("The argument ", sQuote("data"), " must be an edsurvey.data.frame, a light.edsurvey.data.frame. See ",
        dQuote(paste0("Using the ", sQuote("EdSurvey"),
        " Package's getData Function to Manipulate the NAEP Primer Data vignette")),
        " for how to work with data in a light.edsurvey.data.frame."))
    }
    # not expected, but just in case
    stop(paste0(sQuote("data"), " must be one of ", paste(dQuote(classes), collapse=", "), "."))
  }
}
