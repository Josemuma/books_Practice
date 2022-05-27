library(mlr3)
library(mlr)
library(tidyverse)

#  K-Nearest Neighbour ####
#Diabetes  case
data(diabetes, package = "mclust")
diabetesTib <- as_tibble(diabetes)
summary(diabetesTib)
diabetesTib
#Some plots
ggplot(diabetesTib, aes(glucose, insulin, col = class)) +
  geom_point() + theme_bw()

ggplot(diabetesTib, aes(sspg, insulin, col = class)) +
  geom_point() + theme_bw()

ggplot(diabetesTib, aes(sspg, glucose, col = class)) +
  geom_point() + theme_bw()
#With figures
ggplot(diabetesTib, aes(glucose, insulin, shape = class)) +
  geom_point() + theme_bw()
#With figures and color
ggplot(diabetesTib, aes(glucose, insulin, shape = class, col = class)) +
  geom_point() + theme_bw()
#First Training
## 1. Define task
# Data with predictor variables and target var we want to predict
diabetesTask <- makeClassifTask(data = diabetesTib, target = "class")

## 2. Define the learner
#We need: type of classification algorithm
#The algorithm and other options
knn <- makeLearner("classif.knn", par.vals = list("k" = 2)) #first parameter is the type of classifier (knn, reg, etc)
##Train the model
#Combine learner and task
knnModel <- train(knn, diabetesTask)
##Test
#We pass the data unlabeled to predict
## 3. Train model ----- All this allows to a single task many learners || single learner test it with multiple tasks
knnPred <- predict(knnModel, newdata = diabetesTib)
performance(knnPred, measures = list(mmce, acc))
#While really nice measure we evaluate the model performance with data use to predict that was also use to train model

### Cross-Validate kNN Model
diabetesTask
knn
# 1. Holdout C-V.- random  prop de tu data para test y entrenas el modelo con el restante de la data
# luego se pasa el test set through the model y se calcula performance

# 1.1 Resample description
holdout <- makeResampleDesc(method = "Holdout", split = 2/3, stratify = TRUE) #stratify busca se mantengan
# prop de cada clase. Importa mucho en casos que los gpos muy unbalanced.
# 1.2 Now we cross-validate our learner
holdoutCV <- resample(learner = knn, task = diabetesTask, resampling = holdout,
                      measures = list(mmce, acc))
holdoutCV$aggr

###EX. 2 10% split, no stratification
holdoutEx2 <- makeResampleDesc(method = "Holdout", split = 1/10, stratify = FALSE)
holdoutCVEx2 <- resample(learner = knn, task = diabetesTask, resampling = holdoutEx2,
                      measures = list(mmce, acc))

# 1.3 Confusion Matrix.- nos da idea de qué grupos estan siendo clasificados de manera correcta
# y cuales misclasificados
# pred nos da TRUE & Predicted classes
calculateConfusionMatrix(holdoutCV$pred, relative = TRUE) #Correct en diagonal de Abs Conf Mtx

# 2. K-fold C-V.- Aprox randomly split in k equal size chunks, folds; train & pass test 
# through model, record it; different fold as test, until we have used all folds; 
# avg perform metrics as estimate of model perf
# to improve it we shuffle the data and repeat the k-fold
kFold <- makeResampleDesc("RepCV", folds = 10, reps = 50, stratify = TRUE)
kFoldCV <- resample(learner = knn, task = diabetesTask, resampling = kFold, list(mmce, acc))
calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)

###EX. 3 3-fold CV, 5x;  3-fold CV, 500x
kFoldEx31 <- makeResampleDesc("RepCV", folds = 3, reps = 5, stratify = TRUE)
kFoldCVEx31 <- resample(learner = knn, task = diabetesTask, resampling = kFoldEx31, list(mmce, acc))
kFoldEx32 <- makeResampleDesc("RepCV", folds = 3, reps = 500, stratify = TRUE)
kFoldCVEx32 <- resample(learner = knn, task = diabetesTask, resampling = kFoldEx32, list(mmce, acc))

# 3. Leave-on-out C-V-.  Sometimes less variable than k-fold when dataset  is small.
# The key of this method is in that one that is reserved for testing
LOO <- makeResampleDesc(method = "LOO")
LOOCV <- resample(knn, diabetesTask, LOO, list(mmce, acc)) #the acc is more variable than the others
calculateConfusionMatrix(LOOCV$pred, relative = TRUE)

#La eleccion the k tiene un costo beneficio de bias-variance (chico/grande k)
#Define a range of values
knnParamSpace <- makeParamSet(makeDiscreteParam("k", values = 1:10))
#We need to pick a method to search the parameter space. There are many...
gridSearch <- makeTuneControlGrid()
#We C-V to try  the different values
cvForTuning <- makeResampleDesc("RepCV", folds = 10, reps = 20)
# then perform the tunning
tunedK <- tuneParams("classif.knn", task = diabetesTask, resampling = cvForTuning,
                     par.set = knnParamSpace, control = gridSearch)
tunedK
tunedK$x
knnTuninngData <- generateHyperParsEffectData(tunedK)
plotHyperParsEffect(knnTuninngData, x = "k", y = "mmce.test.mean", plot.type = "line") +
                      theme_bw()
#We train the model with the 'ideal' k, 7
tunnedKnn <- setHyperPars(makeLearner("classif.knn"),  par.vals = tunedK$x) #par.vals es el tunned k value
tunnedKnnModel <- train(tunnedKnn, diabetesTask)

#Now we include hyperparameters in the CV
inner <- makeResampleDesc("CV")
outer <- makeResampleDesc("RepCV", folds = 10, reps = 5)
#We make the wrapper (un learner tied to a pre-processing), this case the hyperparameter
knnWrapper <- makeTuneWrapper("classif.knn", resampling = inner,
                              par.set = knnParamSpace, control = gridSearch)
#Se puede ver arriba que esto esta pasando ahora en el inner loop
#Now we run the nested CV procedure
#1st argument wrapper, 2nd name of task, 3rd resampling argument with outer
cvWithTuning <- resample(knnWrapper, diabetesTask, resampling = outer)
cvWithTuning

#Model to predict
newDiabetesPatients <- tibble(glucose = c(82, 108, 300),
                              insulin = c(8361, 288, 1052),
                              sspg = c(200, 186, 135))
newPatientsPred <-  predict(tunnedKnnModel,  newdata = newDiabetesPatients)
getPredictionResponse(newPatientsPred)





