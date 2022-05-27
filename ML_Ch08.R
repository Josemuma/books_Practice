library(pacman)
p_load(mlr3,mlr,e1071,tidyverse,parallel,parallelMap,rpart.plot)

#Good when many trees are combined together. 
ls() #Podemos ver que variables ya tenemos

## Bootstrap Aggregating (bagging)
To reduce noisy data and prediction variance
Decide number of sub-models to train; para cada caso, randomly sample
cases del training set with replacement hasta tener n; train sub-model; 
pass new data thtough each sub-model; output will be the most frequent

## Boosting 
Trains the models in a sequential way. Looking to correct the previous ensemble model.

# Four important hyperp to tune:
ntree - # individual trees in forest
mtry - # of features to randomly sample at each node
nodesize - min # of cases allowed in a leaf (like minbucket)
maxnodes - max # of leaves allowed

# Make the learner
forest <- makeLearner("classif.randomForest")

#Tuning random forest hyperp
Queremos exactamente 300 arboles; no queremos las 16 variables pues no todas sirven (6-12)
al tener grupos pequeños definimos leaves pequeñas con pocos casos (1-5), no limitamos el arbol mucho (5-20)
forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 300, upper = 300),
  makeIntegerParam("mtry", lower =  6, upper = 12),
  makeIntegerParam("nodesize", lower = 1, upper = 5),
  makeIntegerParam("maxnodes", lower = 5, upper = 20))

randSearch <- makeTuneControlRandom(maxit = 100) #100 iterations when searching
cvForTuning <- makeResampleDesc("CV", iters = 5) #5-fold CV
parallelStartSocket(cpus = 5)
tunedForestPars <- tuneParams(forest, task = zooTask,
                              resampling = cvForTuning,
                              par.set = forestParamSpace,
                              control = randSearch)
parallelStop()
tunedForestPars

#We train a last model to make a learner with our tuned hyperp and passing it to train()
tunedForest <- setHyperPars(forest, par.vals = tunedForestPars$x)
tunedForestModel <-  train(tunedForest, zooTask)

#Do we have enough trees?
forestModelData <- getLearnerModel(tunedForestModel)
species <- colnames(forestModelData$err.rate)
plot(forestModelData, col = 1:length(species), lty = 1:length(species))
legend("topright", species,
       col = 1:length(species), lty = 1:length(species))

#Cross-Validating the model 
outer <- makeResampleDesc("CV", iters = 5)
forestWrapper <- makeTuneWrapper("classif.randomForest",
                                 resampling = cvForTuning,
                                 par.set = forestParamSpace,
                                 control = randSearch)
parallelStartSocket(cpus = 5)
cvWithTuning <- resample(forestWrapper,zooTask,resampling = outer)
parallelStop()
cvWithTuning
#Mejoró el mmce mucho contr Ch 7.12

# XGBoost Model

#Create learner
xgb <- makeLearner("classif.xgboost")
#XGBoost can only use numeric values
zooXgb <- mutate_at(zooTib, .vars = vars(-type), .funs = as.numeric)
xgbTask <- makeClassifTask(data = zooXgb, target = "type")

#Tuning XGBoost hyperparameters
xgbParamSpace <- makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeNumericParam("gamma", lower = 0, upper = 5),
  makeIntegerParam("max_depth", lower = 1, upper = 5),
  makeNumericParam("min_child_weight", lower = 1, upper = 10),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  makeIntegerParam("nrounds", lower = 20, upper = 20),
  makeDiscreteParam("eval_metric", values = c("merror", "mlogloss")))
randSearch <- makeTuneControlRandom(maxit = 1000)
cvForTuning <- makeResampleDesc("CV", iters = 5)
tunedXgbPars <- tuneParams(xgb, task = xgbTask,
                           resampling = cvForTuning, 
                           par.set = xgbParamSpace,
                           control = randSearch)
tunedXgbPars
#es mejor empezar de acuerdo a lo que creemos ideal en "nrounds"  e ir subiendo si la computadora lo permite

#Train the final tuned model
tunedXgb <- setHyperPars(xgb, par.vals = tunedXgbPars$x)
tunedXgbModel <- train(tunedXgb, xgbTask)

#Plot Loss function vs Iteration # para ver si necesitamos mas trees
xgbModelData <- getLearnerModel(tunedXgbModel) 
#Hay que recordar que eval metric sale y eso es lo que se pone en aes()
ggplot(xgbModelData$evaluation_log, aes(iter, train_merror)) +
  geom_line() +
  geom_point()

#We can, when we dont have too much trees, plot the individual trees in the ensemble
library(DiagrammeR)
xgboost::xgb.plot.tree(model = xgbModelData, trees = 1:5)
xgboost::xgb.plot.multi.trees(xgbModelData)

#CV the model
outer <- makeResampleDesc("CV", iters = 3)
xgbWrapper <- makeTuneWrapper("classif.xgboost",
                              resampling = cvForTuning,
                              par.set = xgbParamSpace,
                              control = randSearch)
cvWithTuning <- resample(xgbWrapper, xgbTask, resampling = outer)
cvWithTuning

#Benchmarking
learners = list(makeLearner("classif.knn"),
                makeLearner("classif.LiblineaRL1LogReg"),
                makeLearner("classif.svm"),
                tunedTree, tunedForest,  tunedXgb)
benchCV <- makeResampleDesc("RepCV", folds = 10, reps = 5)
bench <- benchmark(learners, xgbTask, benchCV)
bench





























