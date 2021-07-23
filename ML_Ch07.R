library(mlr3)
library(mlr)
library(e1071)
library(tidyverse)
library(parallel)
library(parallelMap)
library(rpart.plot)

# 7.1 Inttroduction
  # Buscamos seleccionar el predictor que mejor discrimina las clases. Para tomar la decision, usamos entropy or Gini Index (es el que usua rpart). Queremos saber que tan heterogenea  es la clase. \\
  # Si partimos de un nodo con clases, esas clases (A,B) deben ser clasificadas. Eventualmente van a una rama y  cada rama tendra alguna cantidad de A y B, al igual que la otra rama, en otra proporcion. Al fina buscamos medir el Gini Gain, la differencia entre el gini index del parent vs el del split.\\
  # GI =  1-(P(A)^2+P(B)^2), las P() son las proporciones de cada clase. \\
  # El  Gini Index del split es la suma ponderada de cada rama * Gini  Index de cada lado. De ese resultado obtenemos el Gini Gain contra el Gini del parent. 
  # 
  # En el caso de valores continuos, los valors en el conjunto de training se ponen en orden de valor
  # y el GiniGain se evalua en el punto medio entre en el par de valores mas cercanos.

## Hiperparametros de 'rpart'
  # Siempre en DT se busca el split que se desempeña mejor en ese split. Local optimal > Global optimal
  # Debemos cuidar que tan deep se hace el arbol. Si seguimos sin parar estariamos en algun punto modelando el ruido.
  # 2 opciones: Crece el arbol y limpialo (prune); Stopping criteria
  # Minimum number of cases in a node before splitting
  # Maximum depth of the tree
  # Minimum improvement in performance for a split
  # Minimum number of cases in a leaf

# 7.3 Loaring Data
  # Con las caracteristicas de un animal, debemos saber cuál es a través de un DT
data(Zoo, package = "mlbench")
zooTib <- as_tibble(Zoo)
zooTib
#Algunas de  las columnas son "Logical" por lo que debemos pasarlas a 'Factor'
zooTib <- mutate_if(zooTib, is.logical, as.factor)

## Training (using rpart)
zooTask <- makeClassifTask(data=zooTib, target = "type")
tree <- makeLearner("classif.rpart") #Es un ej de Clasificacion y usamos rpart

#Tuning parameters
##Definr parameter space for: minsplit, minbucket, cp, maxdepth

###maxcompete cuantos candidate splits displayed para cada node en el resumen.
###El resumen muestra las propuestas de split para ver qué tanto ha mejorado el moodelo. 

###maxsusrrogate controla cuantos surrogate splits se muestran. Un surrogate se uso si hace
###falta data en el split actual. Generalmente se usa 5 como default.

###usesurrogate como usa surrogatet splits el algoritmo. Generlamente 2.

getParamSet(tree)
#Definimos espacio:
treeParamSpace  <- makeParamSet(
  makeIntegerParam("minsplit", lower = 5, upper = 20),
  makeIntegerParam("minbucket", lower = 3, upper = 10),
  makeNumericParam("cp", lower = 0.01, upper = 0.1),
  makeIntegerParam("maxdepth", lower = 3, upper = 10))

#Cuando es muy grande el  espacio combiene buscar de manera aleatoria, no exhaustiva, pero mas rapida
# OJO!!! Si tenemos pocos casos por clase, analizar si vale la pena dejar la clase o quitarla

randSearch <- makeTuneControlRandom(maxit=200)
cvForTuning <- makeResampleDesc("CV", iters = 5)

#Let the tuning begin
parallelStartSocket(cpus = detectCores())#esto lo podemos definr e usar la compu en otra cosa mientras    
tunedTreePars <- tuneParams(tree, task =zooTask,
                            resampling = cvForTuning,
                            par.set = treeParamSpace,
                            control = randSearch) #(learner,task, resampling  CV, ParamSpace, SearchMethod)
parallelStop()
tunedTreePars

#Con los  tuner parametros entrenamos el modelo
tunedTree <- setHyperPars(tree, par.vals = tunedTreePars$x)
tunedtTreeModel <- train(tunedTree, zooTask)
tunedtTreeModel

#Extraemos modelo con getLearnerModel()
treeModelData <- getLearnerModel(tunedtTreeModel)
rpart.plot(treeModelData, roundint = FALSE, box.palette = "BuBn", type = 5)  #Si hay un problema de  colores, solo cambiar box.palette
#Inspeccionando los param
printcp(treeModelData, digits = 3)
#La  formula de cp: 
(1-.667)/(1-0) #y asi sucesivamente
summary(treeModelData)

#Cross-Validation of the model
#OJO!!! No olvidar el uso de data-dependent preprocessing in the CV.
#Incluyendo hyperparameter tuning
outer <- makeResampleDesc("CV", iters = 5)
treeWrapper <- makeTuneWrapper("classif.rpart", resampling = cvForTuning,
                               par.set = treeParamSpace, control = randSearch)
parallelStartSocket(cpus = 4)
cwWithTtuning <- resample(treeWrapper, zooTask, resampling = outer)
parallelStop()
cwWithTtuning

#Podemos apreciar que el mmce en CV es mayor que cuando hicimos tuning. 
#Esto se debe a que hay overfitting. 
#Mejor inccluiur hyperparameter tuning inside the CV procedure

### una mejora son ENSEMBLE METHODS












