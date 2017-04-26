\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# for two categorical variables any of the following work
c1_pears <- cor.sdf("b017451", "b003501", sdf,
  method="Pearson",
  weightVar="origwt")
c1_spear <- cor.sdf("b017451", "b003501", sdf,
  method="Spearman",
  weightVar="origwt")
c1_polyc <- cor.sdf("b017451", "b003501", sdf,
  method="Polychoric",
  weightVar="origwt")

c1_pears
c1_spear
c1_polyc

# these take awhile to calculate for large datasets, so limit to a subset
sdf_dnf <- subset(sdf, b003601 == 1)

# for a categorical variable and a scale score any of the following work
c2_pears <- cor.sdf("composite", "b017451", sdf_dnf,
	method="Pearson",
	weightVar="origwt")
c2_spear <- cor.sdf("composite", "b017451", sdf_dnf,
	method="Spearman",
	weightVar="origwt")
c2_polys <- cor.sdf("composite", "b017451", sdf_dnf,
	method="Polyserial",
	weightVar="origwt")

c2_pears
c2_spear
c2_polys

# recode two variables
cor.sdf("c046501","c044006",sdf, method="Spearman", weightVar = "origwt",
	recode=list(
		c046501=list(from='0%',to='None'),
		c046501=list(
			from=c('1-5%','6-10%',"11-25%",'26-50%','51-75%','76-90%','Over 90%'),
			to='Between 0% and 100%'),
		c044006=list(
			from=c('1-5%','6-10%',"11-25%",'26-50%','51-75%','76-90%','Over 90%'),
			to='Between 0% and 100%')))

# reorder two variables
cor.sdf("b017451","sdracem", sdf, method="Spearman", weightVar="origwt", 
	reorder=list(
		sdracem=c("White","Hispanic","Black","Asian/Pacific Island",
			"Amer Ind/Alaska Natv","Other"),
	  b017451=c("Every day","2 or 3 times a week","About once a week",
			"Once every few weeks","Never or hardly ever")))

# recode two variables and reorder
cor.sdf("pared","b013801",sdf,method="Spearman", weightVar = "origwt",
	recode=list(
		pared=list(from='Some ed after H.S.',to='Graduated H.S.'), 
		pared=list(from='Some ed after H.S.',to='Graduated H.S.'),
		pared=list(from='Graduated college',to='Graduated H.S.'),
		b013801=list(from='0-10',to='Less than 100'), 
		b013801=list(from='11-25',to='Less than 100'),
		b013801=list(from='26-100',to='Less than 100')),
	reorder=list(
		b013801=c('Less than 100','>100')))
}
