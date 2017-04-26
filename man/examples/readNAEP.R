# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))
sdf

\dontrun{
# To read in an NCES file first set the directory to the ~/Data subfolder,
# then read in the appropriate .dat file:
setwd("location/of/Data")
sdf <- readNAEP('M36NT2PM.dat')

# Or read in the .dat file directly through the folder pathway:
sdf <- readNAEP('location/of/DataM36NT2PM.dat')
}