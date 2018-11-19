\dontrun{
# read in the example data (generated, not real student data)
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

# subset to a smaller sample
sdf_subset <- subset(sdf, scrpsu < 500)

# fast is an argument from WeMix::mix that allows the function to run faster using c++
m1 <- mixed.sdf(composite ~ dsex + b017451 + (1|scrpsu), data=sdf_subset,
                weightVar = c('origwt', 'srwt01'),
                fast=TRUE, verbose=1)
summary(m1)

# Run multilevel logistic regression model
# nQuad is specified to be 7, which means 
# the function will use 7 quadrature points for the integration
m2 <- mixed.sdf(I(composite >= 214) ~ (1|scrpsu), 
                data=sdf_subset, family = binomial(link="logit"),
                weightVar = c('origwt', 'srwt01'), 
                nQuad = 7, verbose=1)
summary(m2)
}
