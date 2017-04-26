# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# True
isWeight("origwt", sdf)

# False
isWeight("dsex", sdf)
