library(kknn)
# Regularization
## It helps us avoid having parameters that become too large
We hope to have less variable models, with less variance.
Mostly use in LR for the slope. 
Penalize parameters that are too large and shrinking them to 0.
We do want the benefit of OLS in the testing set but not on the test set.

If we overfit the data when we give new data to estimate/predict, it will 
wrongly genealize the results. We will have the sampling error

It is also important for the prevention of spurious predictors. 

Avoids adding everythinng you think to a model, to cause overfitting. As it will
penalize those factors that do not really add to the model other than overfitting.

Two type of penalties, L1 and L2 norm.

# Ridge Regression
## Remember always why underfit and overfit are problematic. Bias and variance (more sensitive)

# Ex. when we are calculate the residuales of a linear regression, we get the
# c. In ML it is the Loss function of the OLS algorithm
# 
# L2 norm is used in ridge regression to modify slightly the SoS. Include a term 
# that makes the function's value larger, the larger the parameter estimates are. 
# Un termino que hace a la funcion mas grande, mientras mas grandes sean los parametros
# estimados. 
# 1. Calculate SoS
# 2. Calculate L2 norm (si solo hay un predictor, es el cuadrado de la pendiente, si
# tenemos 2 la suma del cuadrado de las pendientes)
# 3. SoS + Lambda * L2 norm
# Mayor lambda, mayor la penalizacion al modelo. 
# Lambda es un hiperparametro, necesario tunear y se puede estimar con CV
# 
# *** Always scale the predictor variables previous to L2 or L1 penalty loss functions
# 
# # LASSO (least absolute shrinkage and selection operator) and L1 norm
# Remember that L2 norm = Sum Coeff^2 (which are the suum of squres loss function)
L1 Norm takes de abs value -> Sum abs(Coeff)
We follow the same steps as with ridge regression. We multiply the penalty by a lambda
We add that term to Sum of Squares (Sum(y-ygorro)^2))
Lasso is good for when we need an algorithm that performs feature selection
Because LASSO can shrink parameters completely to 0

# Elastic Net
Es un punto medio entre LASSO y  ridge. La formula usa un alpha que multiplica
al L1 y (1-alfa) a L2. Despues suma el resultado a SS. Alpha is tuned as an hyperp

# Model
library(mlr3)
library(mlr)
library(tidyverse)
library(lasso2)
library(randomForestSRC)
Tratar de estimar el precio de wheat el siguiente año. Precio depende de la produccion del año.
data(Iowa,  package = "lasso2")
iowaTib <- as_tibble(Iowa)
iowaTib
We look for any linear relationship with the Yield
iowaUntidy <- gather(iowaTib, "Variable", "Value", -Yield) #Gather vuelve la matriz original de 33x10 en una "matriz"
#que repite datos e informacion para cada columna. Columna 1 -> Yield, Columna 2-> Nombre de la variable y C3 -> El año
#Por lo que vamos a tener toda la yield - VarName01 - VarValue01 , al acabar todos los años, los siguientes renglones tienen 
#yield - VarName02 - VarValue02

ggplot(iowaUntidy, aes(Value, Yield)) +
  facet_wrap(~Variable, scales = "free_x") +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()
Regularization nos ayuda a evitar overfitting

#Train Ridge Regression Model
We need to define task and learner. 
'glmnet' nos deja hacer los 3 tipos de modelos del capitulo.
alpha = 0 porque queremos usar Ridge Regression
id nos deja dar un nombre  unico a cada learners (nos sirve para hacer benchmark con los otros modos, sino lo especificamos,
                                                  nos va a arrojar un error)
iowaTask <- makeRegrTask( data = iowaTib, target="Yield")
ridge <- makeLearner("regr.glmnet", alpha = 0, id = "ridge")
#Necesitamos saber cuanto contribuye cada predictor en predecir Yield
filterVals <- generateFilterValuesData(iowaTask)
plotFilterValues(filterVals) + theme_bw()
#Graficamente pudieramos quitar algunos casos ya. 
#Dejaremos que el algoritmo shrink los que menos contribuyan
#Primero TUNE lambda que es la que castiga a los parametros (a mayor lambda mayor shrink a 0 los parametros)

#  'makeParamSet() es la funcion para definir los hyperparametros

ridgeParamSpace <- makeParamSet(makeNumericParam("s", lower = 0, upper = 15))
randSearch <- makeTuneControlRandom(maxit = 200) #Numero de iteraciones
cvForTuning <- makeResampleDesc("RepCV", folds = 3, reps = 10) #CV3-fold repeated 10 times(?)

library(parallel)
library(parallelMap)

parallelStartSocket(cpus = detectCores())
tunedRidgePars <- tuneParams(ridge, task = iowaTask, resampling = cvForTuning, par.set = ridgeParamSpace, control = randSearch)
parallelStop()
tunedRidgePars
#Para comprobar que se busco sobre una buena cantidad de lambdas, graf lambdas vs Mean MSE
#Extraemos Lambda y Mean MSE de cada iteracion
ridgeTuningData <- generateHyperParsEffectData(tunedRidgePars)
#Graficamos los datos
plotHyperParsEffect(ridgeTuningData, x= 's', y = 'mse.test.mean',  plot.type = 'line') +
  theme_bw()

###Repeated exercise with s between 0 and 50  #####
ridgeParamSpace2 <- makeParamSet(makeNumericParam("s", lower = 0, upper = 50))
randSearch2 <- makeTuneControlRandom(maxit = 200) #Numero de iteraciones
cvForTuning2 <- makeResampleDesc("RepCV", folds = 3, reps = 10) #CV3-fold repeated 10 times(?)

parallelStartSocket(cpus = detectCores())
tunedRidgePars2 <- tuneParams(ridge, task = iowaTask, resampling = cvForTuning2, par.set = ridgeParamSpace2, control = randSearch2)
parallelStop()
tunedRidgePars2
#Para comprobar que se busco sobre una buena cantidad de lambdas, graf lambdas vs Mean MSE
#Extraemos Lambda y Mean MSE de cada iteracion
ridgeTuningData2 <- generateHyperParsEffectData(tunedRidgePars2)
#Graficamos los datos
plotHyperParsEffect(ridgeTuningData2, x= 's', y = 'mse.test.mean',  plot.type = 'line') +
  theme_bw()
#####

#Train the model
#Definimos un nuevo learner con la lambda nueva. 
tunedRidge <- setHyperPars(ridge, par.vals = tunedRidgePars$x)
tunedRidgeModel <- train(tunedRidge, iowaTask)
#Extract the paramer estimates
ridgeModelData <- getLearnerModel(tunedRidgeModel)
#Extrac coefficients
ridgeCoefs <- coef(ridgeModelData, s = tunedRidgePars$x$s)
ridgeCoefs
#Plot estimates vs unregularized linear regression
lmCoefs <- coef(lm(Yield ~., data = iowaTib))
coefTib <- tibble(Coef = rownames(ridgeCoefs)[-1],
                  Ridge = as.vector(ridgeCoefs)[-1],
                  Lm = as.vector(lmCoefs)[-1])
coefUntidy <- gather(coefTib, key  = Model, value =  Beta, -Coef)
ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill =  Model)) +
  geom_bar(stat = "identity", col = "black")+
  facet_wrap(~Model)+
  theme_bw() + theme(legend.position = "none")

##Lasso Model Training
#Definimos el learner de LASSO y  el alfa = 1 (pure LASSO)
lasso <- makeLearner("regr.glmnet", alpha = 1, id = "lasso")
#We tuned lambda
lassoParamSpace <- makeParamSet( makeNumericParam("s", lower = 0, upper = 15))
parallelStartSocket(cpus = detectCores())
tunedLassoPars <- tuneParams(lasso, task = iowaTask, 
                             resampling = cvForTuning, par.set = lassoParamSpace, control = randSearch)
parallelStop()
tunedLassoPars
#Revisamos si necesitamos mas busqueda
lassoTuningData <- generateHyperParsEffectData(tunedLassoPars) 
plotHyperParsEffect(lassoTuningData, x = "s", y = "mse.test.mean", plot.type = "line") + 
  theme_bw()
#MSE Mean se hace plana a partir de 10 ya  que la penalizacion es tan grande que quita todos los predictors
#Train Lasso
tunedLasso <- setHyperPars(lasso, par.vals = tunedLassoPars$x)
tunedLassoModel <- train(tunedLasso, iowaTask)
#Comparamos con el tradicional OLS
lassoModelData <- getLearnerModel(tunedLassoModel)
lassoCoefs <- coef(lassoModelData, s = tunedLassoPars$x$s)
lassoCoefs #3 parametros se hicieron 0, es una   caracteristica de LASSo al hacer feature selection
#Agregamos columnas con los valores de Ridge y OLS
coefTib$LASSO <- as.vector(lassoCoefs)[-1]
coefUntidy <- gather(coefTib, key = Model, value = Beta, -Coef)
ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill = Model)) + 
  geom_bar(stat = "identity", col = "black") + 
  facet_wrap(~ Model) + theme_bw() + 
  theme(legend.position = "none")

##Elastic Net Model
#Necesitamos tuning the alpha y lambda
#No vamos a dar valor de alpha ya que con tuning encontraremos el mejor trade-off entre L1 & L2 regularization
elastic <- makeLearner("regr.glmnet", id = "elastic")
#Definimos space Hyperparameter
elasticParamSpace <- makeParamSet(makeNumericParam("s", lower = 0, upper = 10), 
                                  makeNumericParam("alpha", lower = 0, upper = 1))
randSearchElastic <- makeTuneControlRandom(maxit = 400)
parallelStartSocket(cpus = detectCores())
tunedElasticPars <- tuneParams(elastic, task = iowaTask, resampling = cvForTuning, 
                               par.set = elasticParamSpace, control = randSearchElastic)
parallelStop()
tunedElasticPars
#Para comprobar que nuestra busqueda fue suficiente, graficamos. Esta ocasion  con 2 variables y MSE Mean
HeatMap nos lleva el color hacie la variable que este en z-axis.
La interpolacion de este metodo es para verlo graficamente, una interpolacion distinta cambia la imagen pero
no el resultado final.
elasticTuningData <- generateHyperParsEffectData(tunedElasticPars)
plotHyperParsEffect(elasticTuningData, x = "s", y = "alpha",
                    z = "mse.test.mean", interpolate = "regr.kknn", plot.type = "heatmap") + 
  scale_fill_gradientn(colours = terrain.colors(5)) + 
  geom_point(x = tunedElasticPars$x$s, y = tunedElasticPars$x$alpha, col = "white") + 
  theme_bw()

plotHyperParsEffect(elasticTuningData, x = "s", y = "alpha",
                    z = "mse.test.mean", interpolate = "regr.kknn", plot.type = "contour", show.experiments = TRUE) + 
  scale_fill_gradientn(colours = terrain.colors(5)) + 
  geom_point(x = tunedElasticPars$x$s, y = tunedElasticPars$x$alpha, col = "white") + 
  theme_bw()

plotHyperParsEffect(elasticTuningData, x = "s", y = "alpha",
                    z = "mse.test.mean", plot.type = "scatter") + 
  geom_point(x = tunedElasticPars$x$s, y = tunedElasticPars$x$alpha, col = "white") + 
  theme_bw()

#Train the Elastic net model
tunedElastic <- setHyperPars(elastic, par.vals = tunedElasticPars$x)
tunedElasticModel <- train(tunedElastic, iowaTask)
#Compare with the other models
elasticModelData <- getLearnerModel(tunedElasticModel)
elasticCoefs <- coef(elasticModelData, s = tunedElasticPars$x$s) 
coefTib$Elastic <- as.vector(elasticCoefs)[-1] 
coefUntidy <- gather(coefTib, key = Model, value = Beta, -Coef) 
ggplot(coefUntidy, aes(reorder(Coef, Beta), Beta, fill = Model)) + 
  geom_bar(stat = "identity", position = "dodge", col = "black") +
  facet_wrap(~ Model) + theme_bw()

Se parece mas a LASSO porque la alfa que estimamos resultó cercana a 1.

#Benchmarking
Tomar una lista de learners, a task, CV procedure.
Por cada iteracion/fold en CV un modelo es entrenado usando el mismo training set y  es evaluado en el mismo test set.
Resultado es el mean performance metric para cada learner.

Definimos tunning wrappers para cada caso. Para cada wrapper damos un learner, strategia de CV, espacio  parametrico para el learner
y search procedure. 
OLS no necesita tuning hyperparametros.

ridgeWrapper <- makeTuneWrapper(ridge, resampling = cvForTuning, par.set = ridgeParamSpace, control = randSearch)
lassoWrapper <- makeTuneWrapper(lasso, resampling = cvForTuning, par.set = lassoParamSpace, control = randSearch)
elasticWrapper <- makeTuneWrapper(elastic, resampling = cvForTuning, par.set = elasticParamSpace, control = randSearchElastic)
Benchmark necesita una lista de learners:
learners = list(ridgeWrapper, lassoWrapper, elasticWrapper, "regr.lm")
Definimos nuestro outer resampling strategy una 3-fold CV. En cuanto inicia la parallelization corremos el benchmark experiment
dandole la lista de learners, task y outer cv.

kFold3 <- makeResampleDesc("CV", iters = 3)
parallelStartSocket(cpus = detectCores())
bench <- benchmark(learners, iowaTask, kFold3)
parallelStop()
bench


















