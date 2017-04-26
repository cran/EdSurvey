# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# search both the student and school files by a character string
searchSDF("book",sdf)

# search only the student files by a character string
searchSDF("algebra",sdf, fileFormat="student")

# search both the student and school files and return a glimpse of levels
searchSDF("value",sdf, levels=TRUE)

# save the search as an object to return a full data.frame of search
ddf <- searchSDF("value",sdf, levels=TRUE)
ddf