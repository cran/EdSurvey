# @author Ahmad Emad
descriptionOfFile <- function(filename) {
  # parse NAEP file names
  filename <- casefold(filename, upper = TRUE)
  file <- filename
  filename <- substring(filename, 1:nchar(filename), 1:nchar(filename))
  if(length(filename) != 8) 
    return("Sorry, no information available about this file")
  meaning_of_digits <- vector(mode = 'list', length = 6)
  # There are 8 digits in a NAEP Filename. Defining each 
  # digit here.
  
  names(meaning_of_digits) <- c('1','23','4','5','6','78')
  meaning_of_digits[['1']] <- "Subject"
  meaning_of_digits[['23']] <- "Year"
  meaning_of_digits[['4']] <- "Assessment_Code"
  meaning_of_digits[['5']] <- "Data_Type"
  meaning_of_digits[['6']] <- "Grade_Level"
  meaning_of_digits[['78']] <- "Assessment_Sample"
  
  Subject <- vector(mode='list', length = 0)
  Subject[['C']] <- 'Civics'
  Subject[['E']] <- 'Economics'
  Subject[['G']] <- 'Geography'
  Subject[['H']] <- 'History'
  Subject[['M']] <- 'Mathematics'
  Subject[['R']] <- 'Reading'
  Subject[['S']] <- 'Science'
  Subject[['U']] <- 'Music'
  Subject[['V']] <- 'Visual Arts'
  Subject[['W']] <- 'Writing'
  
  Assessment_Code <- vector(mode = 'list', length = 0)
  Assessment_Code[['L']] <- "Long-Term Trend"
  Assessment_Code[['N']] <- 'National'
  Assessment_Code[['S']] <- 'State'
  
  Data_Type <- vector(mode = 'list', length = 0)
  Data_Type[['T']] <- 'Student Data'
  Data_Type[['C']] <- 'School Data'
  
  Assessment_Sample <- vector(mode = 'list', length = 0)
  Assessment_Sample[['AT']] <- 'Total Sample'
  Assessment_Sample[['RT']] <- 'Modified Sample'
  
  Grade_Level <- vector(mode = 'list', length = 0)
  Grade_Level[['L_1']] <- 'Age 9'
  Grade_Level[['L_2']] <- 'Age 13'
  Grade_Level[['L_3']] <- 'Age 17'
  Grade_Level[['N_1']] <- 'Grade 4'
  Grade_Level[['N_2']] <- 'Grade 8'
  Grade_Level[['N_3']] <- 'Grade 12'
  Grade_Level[['S_1']] <- 'Grade 4'
  Grade_Level[['S_2']] <- 'Grade 8'
  Grade_Level[['S_3']] <- 'Grade 12'
  
  year <- as.numeric(paste(filename[2],filename[3],sep='')) + 1969
  codes <- c(filename[1],
             paste(filename[2],
                   filename[3],
                   sep=''),
             filename[4],
             filename[5],
             paste(filename[4],
                   filename[6],
                   sep='_'),
             paste(filename[7],
                   filename[8],
                   sep=''))
  
  attributes <- vector(mode = 'list', length = 0)
  for (i in 1:6) {
    code <- codes[i]
    area <- meaning_of_digits[[names(meaning_of_digits)[i]]]
    
    if(area!='Year') {
      temp <- get(area)[[code]]
      attributes[[area]] <- temp 
    }
    else attributes[[area]] <- year
  }
  attributes[['filename']] <- file
  return(attributes)  
}

achievementLevelsHelp <- function(grade, year, subject) {
  # return achievement levels
  temp <- "Mathematics	1990-present	Grade 4	214	249	282
  Mathematics	1990-present	Grade 8	262	299	333
  Mathematics	1990-2003	Grade 12	288	336	367
  Mathematics	2005-present	Grade 12	141	176	216
  Reading	1992-2007	Grade 4	208	238	268
  Reading	2009-present	Grade 4	208	238	268
  Reading	1992-2007	Grade 8	243	281	323
  Reading	2009-present	Grade 8	243	281	323
  Reading	1992-2007	Grade 12	265	302	346
  Reading	2009-present	Grade 12	265	302	346
  Science	1990-2005	Grade 4	138	170	205
  Science	2009-2011	Grade 4	131	167	224
  Science	1990-2005	Grade 8	143	170	208
  Science	2009-2011	Grade 8	141	170	215
  Science	1990-2005	Grade 12	146	178	210
  Science	2009-2011	Grade 12	142	179	222
  Writing	1998-2007	Grade 4	115	176	225
  Writing	2011	Grade 4	115	176	225
  Writing	1998-2007	Grade 8	114	173	224
  Writing	2011	Grade 8	120	173	211
  Writing	1998-2007	Grade 12	122	178	230
  Writing	2011	Grade 12	122	173	210
  Civics	all	Grade 4	136	177	215
  Civics	all	Grade 8	134	178	213
  Civics	all	Grade 12	139	174	204
  Geography	all	Grade 4	187	240	276
  Geography	all	Grade 8	242	282	315
  Geography	all	Grade 12	270	305	339
  History	all	Grade 4	195	243	276
  History	all	Grade 8	252	294	327
  History	all	Grade 12	294	325	355
  Economics	all	Grade 12	123	160	208
  "
  temp2 <- unlist(strsplit(temp,"\n"))
  temp2 <- temp2[1:32]
  temp2 <- sapply(temp2, function(x) unlist(strsplit(x,"\t")),
                  USE.NAMES = FALSE)
  temp2 <- data.frame(matrix(unlist(temp2), nrow = 32, byrow = TRUE))
  names(temp2) <- c("Subject",	"Year",	"Grade",	"Basic",	"Proficient",	"Advanced")
  temp2[,"Subject"] <- gsub(" ","",temp2$Subject)
  #subject <- tolower(subject)
  #grade <- tolower(grade)
  
  levels <- temp2[temp2$Subject == subject & temp2$Grade == grade,]
  if(nrow(levels) > 1) {
    for (i in seq(1,nrow(levels))) {
      y <- as.character(levels$Year[i])
      if(length(strsplit(y, "-")[[1]]) == 1) {
        
        if(y == year) 
          return (c(as.character(levels$Basic)[i], 
                    as.character(levels$Proficient)[i], as.character(levels$Advanced)[i]))
        next
      }
      
      lower <- as.integer(strsplit(y, "-")[[1]][1])
      upper <- strsplit(y, "-")[[1]][2]
      
      if(upper == "present") 
        upper <- substr(Sys.Date(),1,4)
      upper <- as.integer(upper)
      if(year >= lower & year <= upper) {
        return (c(as.character(levels$Basic)[i], 
                  as.character(levels$Proficient)[i], as.character(levels$Advanced)[i]))
      }
      
    }
  }
  if(nrow(levels) == 1) {
    return (c(as.character(levels$Basic), 
              as.character(levels$Proficient), as.character(levels$Advanced)))
  }
  return (rep("Not found", 3))
}
