library(abn)
# Load the data for this exercise
data("asia", package = "bnlearn")
# Rename the columns 
colnames(asia) <- c("Asia", "Smoking", "Tuberculosis", "LungCancer",
                    "Bronchitis", "Either", "XRay", "Dyspnea")
# Evaluate the relationship between variables
# Given the data, each variable is distributed Binomial (1,0)
distrib <- as.list(rep("binomial", 8)) 
names(distrib) <- names(asia)
# This is to set the underlying relationship between variables
# Give a DF, distributions and number of parents
mycache <- buildScoreCache(data.df = asia, data.dist = distrib,
                           max.parents = 4)
# This is to find the must probable DAG using Koivisto & Sood (04)
mp.dag <- mostProbable(score.cache = mycache)
# Plotting the results
fabn <- fitAbn(object = mp.dag, create.graph = TRUE) 
# DAG (This example highlights that the R package abn does not model causality)
require(Rgraphviz)
plot(fabn$abnDag)

# Other option
mycache <- buildScoreCache(data.df = asia, data.dist = distrib,
                           max.parents = 4, dag.retained = ~LungCancer | Smoking)
mp.dag <- mostProbable(score.cache = mycache) 
fabn <- fitAbn(object = mp.dag, create.graph = TRUE)
fabn

rm(distrib, mycache, mp.dag, fabn)
