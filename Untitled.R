library(tidyverse)
library(rstanarm)
a <- t(matrix(rep(c(0,-1/12/100,1/12/100), times = c(1400*.75,1400*.23,1400*.02)),nrow = 1400, ncol = 384))
e <- replicate(1400,rnorm(384,mean = 0,sd = 0.021))
r = a + e
r <- as.data.frame(r)
fafr <- read.csv("FourFactors.csv")
fafr <- fafr %>% map_df(rev)
fafr <- fafr[,c(1,2,3,6,5,4)]

results <- sapply(1:1, function(x) stan_glm(r[,1400] ~ ., data = fafr[,1:4])) #here weak prior will be used, that partially pool
#the coeff towards 0
#We can instead, use flat priors, so that posteiror distr is the same as the likelihood
#We set prior_intercept = NULL as well as prior & prior_aux
#We can optimize too, algorithm = "optimizing"
summary(b)
prior_summary(b)
as.matrix(b)[,1] #extract posteriors simulations
mean(as.matrix(b)[,1])
