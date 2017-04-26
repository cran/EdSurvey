\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# By default uses jacknife variance method using replicate weights
lm1 <- lm.sdf(composite ~ dsex + b017451, sdf)
lm1

# for more detailed results use summary:
summary(lm1)

# to specify a variance method use varMethod:
lm2 <- lm.sdf(composite ~ dsex + b017451, sdf,
	varMethod="Taylor")
lm2
summary(lm2)

# Use relevel to set a new omitted category.
lm3 <- lm.sdf(composite ~ dsex + b017451, sdf,
	relevels=list(
	dsex="Female"))
summary(lm3)

# Use recode to change values for specified variables:
lm4 <- lm.sdf(composite ~ dsex + b017451, sdf,
	recode=list(
		b017451=list(
			from=c("Never or hardly ever",
				"Once every few weeks","About once a week"),
			to=c("Infrequently")),
		b017451=list(
			from=c("2 or 3 times a week","Every day"),
			to=c("Frequently"))))
# Note: "Infrequently" is the dropped level for the recoded b017451
summary(lm4)

}