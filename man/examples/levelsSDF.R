# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# search variables in the sdf
levelsSDF("pared",sdf)

# search multiple variables
levelsSDF(c("pared","ell3"),sdf)

# search multiple variables in a light.edsurvey.data.frame with recodes
df2 <- getData(sdf, c("dsex", "t088301"),
	recode=list(
		t088301=list(
			from=c("Yes, available","Yes, I have access"),
			to=c("Yes")),
		t088301=list(from=c("No, have no access"),
			to=c("No"))),
	addAttributes=TRUE)
levelsSDF(c("dsex","t088301"),df2)
