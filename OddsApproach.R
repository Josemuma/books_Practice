##### Load Packages #####
library(fitdistrplus)
library(RColorBrewer)
library(ggplot2)
library(bayesboot)
library(boot)
library(DirichletReg)
library(extraDistr)
#####

xx <- readRDS("xData.rds")
xpost <- readRDS("xPost.rds")

length(x[,1])
sampletemp <- as.data.frame(sample(xpost[,1],384,replace = TRUE, 
                                prob = DirichletReg::rdirichlet(1,c(rep(1,4000)))))
negtemp = sum(sampletemp < (-0.025/12)) 
postemp = sum(sampletemp > (0.025/12))
ratio = (negtemp + postemp)/384

#####
#####
#Beta odd version with alphas
alphadf <- as.data.frame(0)
for (i in 1:1400) {
  sampletemp <- as.data.frame(sample(xpost[i,],1400,replace = TRUE, 
                                     prob = DirichletReg::rdirichlet(1,c(rep(1,1400)))))
  negtemp = sum(sampletemp < (-0.025/12)) 
  postemp = sum(sampletemp > (0.025/12))
  alphadf[i,1] <- negtemp + postemp #"wins"
  alphadf[i,2] <- (negtemp + postemp)/1400  #ratio
  
  sampletemp2 <- as.data.frame(sample(xpost[i,],1400,replace = TRUE)) #Traditional Bootstrap
  negtemp2 = sum(sampletemp2 < (-0.025/12)) 
  postemp2 = sum(sampletemp2 > (0.025/12))
  alphadf[i,3] <- negtemp2 + postemp2 #"wins"
  alphadf[i,4] <- (negtemp2 + postemp2)/1400  #ratio
} 
rm(sampletemp, sampletemp2, postemp, postemp2, negtemp, negtemp2)
colMeans(alphadf)
colnames(alphadf) <- c('NoOfNonZeroBB', 'RatioBB', 'NoOfNonZeroTB', 'RatioTB')
head(alphadf)

fitBeta <- fitdist(alphadf$RatioBB,distr = "beta")  #Adjusted values for the beta distribution
fitBeta2  <- fitdist(alphadf$RatioTB,distr = "beta")  #Adjusted values for the beta distribution
fitBeta
fitBeta2
plot(density((alphadf$RatioTB)))
lines(density(alphadf$RatioBB), col = 'red') #BB
hist(alphadf$RatioTB)
hist(alphadf$RatioBB, add =  TRUE, col = 'red')

plot(density(dbeta(alphadf$NoOfNonZeroBB, fitBeta$estimate[1],fitBeta$estimate[2])), col = 'green')
lines(density(dbeta(alphadf$NoOfNonZeroTB, fitBeta2$estimate[1],fitBeta2$estimate[2])), col = 'blue') #they are the same


prior =fitBeta$estimate[1] /(fitBeta$estimate[2]+fitBeta$estimate[1])
alphadf$Posterior =  alphadf$`0` + fitBeta$estimate[1]
alphadf$PosteriorBeta =  1400 -  alphadf$`0` + fitBeta$estimate[1]
fitBeta2 <- fitdist(alphadf$Posterior,distr = "beta")


mean(sapply(seq(0.50,0.75,0.05), FUN=function(x) (sum(dgp2_25[sample(seq(4,4000,4),1),]>x))/((1-x)*1400)))
mean(sapply(seq(0.50,0.75,0.05), FUN=function(x) (sum(dgp2_35[sample(seq(4,4000,4),1),]>x))/((1-x)*1400)))
#Pi de bayes regresion basica
sapply(seq(0.50,0.75,0.05), FUN=function(x) (sum(pd_to_p(xx$PD)>.6))/((1-x)*1400))
#Calculo de p-value BB
pd_to_p(pd(sample(xpost[,1],1400,replace = TRUE, prob =DirichletReg::rdirichlet(1,c(rep(1,4000))))))
#Calculo de p-value TB 
pd_to_p(pd(sample(xpost[,1],384,replace = TRUE)))

##### ##### Exampled baseball ##### #####

plot(seq(0,0.5,.0001),dbeta(seq(0,0.5,.0001),81,219),  col =  'blue')
lines(seq(0,0.5,.001),dbeta(seq(0,0.5,.001),82,219), col = 'red') #Solo actualizamos después de una vez
lines(seq(0,0.5,.001),dbeta(seq(0,0.5,.001),181,419), col = 'green')#actualizamos 300 tiros después
#The expected values: alfa / (alfa + beta)
#definiendo de la muestra en exitos y fracasos
c(81/(81+219), 82/(82+219), 181/(181+419))
plot(seq(0,0.5,.001),dbeta(seq(0,0.5,.001),350,1050), col = 'blue')
lines(seq(0,0.5,.001),dbeta(seq(0,0.5,.001),300,1050), col = 'red')
lines(seq(0,0.5,.001),dbeta(seq(0,0.5,.001),400,1050), col = 'green')
#definiendo la media en 0.25
plot(seq(0,0.5,.001),dbeta(seq(0,0.5,.001),49,150), col = 'blue')
lines(seq(0,0.5,.001),dbeta(seq(0,0.5,.001),39,150), col = 'red')
lines(seq(0,0.5,.001),dbeta(seq(0,0.5,.001),59,150), col = 'green')
#definiendo la media en 0.75
plot(seq(0.65,.85,.001),dbeta(seq(0.65,.85,.001),224,74), col = 'blue')
lines(seq(0.65,.85,.001),dbeta(seq(0.65,.85,.001),214,74), col = 'red')
lines(seq(0.65,.85,.001),dbeta(seq(0.65,.85,.001),254,74), col = 'green')
##### #####

##### #####
#Beta odd version with p-values
pValuedf <- as.data.frame(0)
minValue = 0.10
lamb = 0.5
for (i in 1:1000) {
  sampletemp <- as.data.frame(sample(dgp2_25[i*4,],1400,replace = TRUE, 
                                     prob = DirichletReg::rdirichlet(1,c(rep(1,1400)))))
  pValuedf[i,1] = sum(sampletemp > minValue) 
  #pValuedf[i,2] <- (sum(sampletemp > minValue))/1400  #ratio
  #metodo de FDR
  pValuedf[i,2] <- (sum(sampletemp > lamb))/((1-lamb)*1400)  #ratio
  
  
  sampletemp2 <- as.data.frame(sample(dgp2_25[i*4,],1400,replace = TRUE)) #Traditional Bootstrap
  pValuedf[i,3] = sum(sampletemp2 > minValue) 
  # pValuedf[i,4] <- (sum(sampletemp2 > minValue))/1400  #ratio
  #metodo de FDR
  pValuedf[i,4] <- (sum(sampletemp2 > lamb))/((1-lamb)*1400)  #ratio
  
} 
rm(sampletemp, sampletemp2)
colMeans(pValuedf)
colnames(pValuedf) <- c('NoOfNonZeroBB', 'RatioBB', 'NoOfNonZeroTB', 'RatioTB')
head(pValuedf)
#Vamos a  ajustar una Beta a los ratios 
fitBeta <- fitdist(pValuedf$RatioBB,distr = "beta")  #Adjusted values for the beta distribution
fitBeta2  <- fitdist(pValuedf$RatioTB,distr = "beta")  #Adjusted values for the beta distribution
fitBeta
fitBeta2
#Aqui los estimadores que consideramos ideales para estar en la media de 75%
fitbetaIdeal <- c(225,  75)
#Asi se ven las densities de los ratios previo a ser ajustadas
plot(density((pValuedf$RatioTB)))
lines(density(pValuedf$RatioBB), col = 'red') #BB
hist(pValuedf$RatioTB)
hist(pValuedf$RatioBB, add =  TRUE, col = 'red')
abline(v=0.75, col = 'black')

#Creamos un vector de random values dado los parametros de la beta ajustada por TB
hist(pValuedf$RatioTB)
hist(rbeta(length(pValuedf$RatioBB),fitBeta2$estimate[1],fitBeta2$estimate[2]), col = 'blue', add = TRUE) #they are the same
hist(rbeta(length(pValuedf$RatioBB),fitbetaIdeal[1],fitbetaIdeal[2]), col = 'red', add = TRUE) #Los ideal

#BB
hist(pValuedf$RatioBB)
hist(rbeta(length(pValuedf$RatioBB),fitBeta$estimate[1],fitBeta$estimate[2]), col = 'blue', add = TRUE) #they are the same
hist(rbeta(length(pValuedf$RatioBB),fitbetaIdeal[1],fitbetaIdeal[2]), col = 'red', add = TRUE) #Los ideal

#We compute the average 'win' ratio under the fitted prior
priorFittedBB <- fitBeta$estimate[1] /  (fitBeta$estimate[1] + fitBeta$estimate[2])
priorFittedTB <- fitBeta2$estimate[1] /  (fitBeta2$estimate[1] + fitBeta2$estimate[2])
priorFitterIdeal  <- fitbetaIdeal[1] /(fitbetaIdeal[1]+ fitbetaIdeal[2]) #El prior que nos gustaría tener

## Update beliefs per strategy. Now each strategy has its own alfa and beta parameters
pValuedf$NoOfNonZeroPostBB <- fitBeta$estimate[1] + pValuedf$NoOfNonZeroBB #alpha posterior
pValuedf$NoOfNonZeroPostTB <- fitBeta2$estimate[1] + pValuedf$NoOfNonZeroTB #alpha posterior
pValuedf$NoOfNonZeroPostBBId <- fitbetaIdeal[1] + pValuedf$NoOfNonZeroBB #alpha posterior ideal
pValuedf$NoOfNonZeroPostTBId <- fitbetaIdeal[1] + pValuedf$NoOfNonZeroTB #alpha posterior ideal


pValuedf$NoOfNonZeroPostBetaBB <- (1400 - pValuedf$NoOfNonZeroBB) + fitBeta$estimate[2] #beta posterior
pValuedf$NoOfNonZeroPostBetaTB <- (1400 - pValuedf$NoOfNonZeroTB) + fitBeta2$estimate[2] #beta posterior
pValuedf$NoOfNonZeroPostBetaBBId <- (1400 - pValuedf$NoOfNonZeroBB) + fitbetaIdeal[2] #beta posterior IDEAL
pValuedf$NoOfNonZeroPostBetaTBId <- (1400 - pValuedf$NoOfNonZeroTB) + fitbetaIdeal[2] #beta posterior IDEAL

#Comprobamos en grafica solo una estrategia (row) vs lo que teniamos antes
hist(rbeta(length(pValuedf$RatioBB),fitBeta$estimate[1],fitBeta$estimate[2]), col = 'blue')
hist(rbeta(length(pValuedf$RatioBB),sample(pValuedf$NoOfNonZeroPostBB,1),sample(pValuedf$NoOfNonZeroPostBetaBB,1)), add = TRUE, col='red')
hist(rbeta(length(pValuedf$RatioBB),sample(pValuedf$NoOfNonZeroPostBBId,1),sample(pValuedf$NoOfNonZeroPostBetaBBId,1)), add = TRUE)

#TB
hist(rbeta(length(pValuedf$RatioBB),fitBeta2$estimate[1],fitBeta2$estimate[2]), col = 'blue')
hist(rbeta(length(pValuedf$RatioBB),sample(pValuedf$NoOfNonZeroPostTB,1),sample(pValuedf$NoOfNonZeroPostBetaTB,1)), add = TRUE, col = 'red')
hist(rbeta(length(pValuedf$RatioBB),sample(pValuedf$NoOfNonZeroPostTBId,1),sample(pValuedf$NoOfNonZeroPostBetaTBId,1)), add = TRUE)

#Shrinkage #We want to visualize it
# The mean posterior 'win' ratio
pValuedf$MeanPostBB <- pValuedf$NoOfNonZeroPostBB / (pValuedf$NoOfNonZeroPostBB + pValuedf$NoOfNonZeroPostBetaBB)
pValuedf$MeanPostTB <- pValuedf$NoOfNonZeroPostTB / (pValuedf$NoOfNonZeroPostTB + pValuedf$NoOfNonZeroPostBetaTB)
pValuedf$MeanPostBBId <- pValuedf$NoOfNonZeroPostBBId / (pValuedf$NoOfNonZeroPostBBId + pValuedf$NoOfNonZeroPostBetaBBId)
pValuedf$MeanPostTBId <- pValuedf$NoOfNonZeroPostTBId / (pValuedf$NoOfNonZeroPostTBId + pValuedf$NoOfNonZeroPostBetaTBId)

hist(pValuedf$RatioBB)
hist(pValuedf$MeanPostBB, add = TRUE, col = 'blue')
hist(pValuedf$MeanPostBBId, add = TRUE, col = 'red')

hist(pValuedf$RatioTB)
hist(pValuedf$MeanPostTB, add = TRUE, col = 'blue')
hist(pValuedf$MeanPostTBId, add = TRUE, col = 'red')

# Now  we  try to see the amount of data shrinkage
scatter.smooth(pValuedf$RatioBB,y = pValuedf$MeanPostBB)
abline(h = priorFittedBB, col = 'red')
scatter.smooth(pValuedf$RatioTB,y = pValuedf$MeanPostTB)
abline(h = priorFittedTB, col = 'red')
scatter.smooth(pValuedf$RatioBB,y = pValuedf$MeanPostBBId)
abline(h = priorFittedBB, col = 'red')
scatter.smooth(pValuedf$RatioTB,y = pValuedf$MeanPostTBId)
abline(h = priorFittedTB, col = 'red')
rbPal <- sort(rnorm(1000))
qplot(pValuedf$RatioBB,pValuedf$MeanPostBB, colour = rbPal) + scale_colour_gradient(low = "red", high = "black")
qplot(pValuedf$RatioTB,pValuedf$MeanPostTB, colour = rbPal) + scale_colour_gradient(low = "green", high = "black") +
  geom_hline(yintercept= priorFittedTB, linetype="dashed", color = "red") +
  geom_abline(slope=1, intercept=0, color = "red")

qplot(pValuedf$RatioTB,pValuedf$MeanPostTBId, colour = rbPal) + scale_colour_gradient(low = "green", high = "black") +
  geom_hline(yintercept= priorFittedTB, linetype="dashed", color = "red") +
  geom_abline(slope=1, intercept=0, color = "red")

#FDR Finally?
#que % de nuestra posterior belief distrib para nuestra estrategia es menor a 0.25
########  Ejex <- seq(0.15,0.45, 5e-04) #1,000 valores entre ese rango ######## 
# plot(Ejex, dbeta(Ejex, 60,140))
# plot(Ejex, dbeta(Ejex, 60,140))
# abline(v = 0.25, col = 'red')
######## 
#Buscamos calcular el area a la izquierda de la linea roja
# Prob de que sea un  FD pbeta(0.25, 60,140)

#Lo revisamos para cada una de las estrategias
pValuedf$FDRBB <- pbeta(0.75,pValuedf$NoOfNonZeroPostBB, pValuedf$NoOfNonZeroPostBetaBB)
pValuedf$FDRTB <- pbeta(0.75,pValuedf$NoOfNonZeroPostTB, pValuedf$NoOfNonZeroPostBetaTB)
pValuedf$FDRBBId <- pbeta(0.75,pValuedf$NoOfNonZeroPostBBId, pValuedf$NoOfNonZeroPostBetaBBId)
pValuedf$FDRTBId <- pbeta(0.75,pValuedf$NoOfNonZeroPostTBId, pValuedf$NoOfNonZeroPostBetaTBId)

(sum(pValuedf$FDRBB > 0.04))/1000
(sum(pValuedf$FDRTB > 0.04))/1000
(sum(pValuedf$FDRBBId > 0.04))/1000
(sum(pValuedf$FDRTBiD > 0.04))/1000
sum(pValuedf$FDRBB < 0.10)/1000
sum(pValuedf$FDRTB < 0.10)/1000
sum(pValuedf$FDRBBId < 0.10)/1000
sum(pValuedf$FDRTBId < 0.10)/1000

sapply(seq(0.05,.9,0.05), FUN = function(x) (sum(pValuedf$FDRBB < x))/((1-x)*1000))
sapply(seq(0.05,.9,0.05), FUN = function(x) (sum(pValuedf$FDRTB < x))/((1-x)*1000))
sapply(seq(0.05,.9,0.05), FUN = function(x) (sum(pValuedf$FDRBBId < x))/((1-x)*1000))
sapply(seq(0.05,.9,0.05), FUN = function(x) (sum(pValuedf$FDRTBId < x))/((1-x)*1000))
sapply(seq(0.5,.8,0.05), FUN = function(x) (sum(dgp2_25[sample(seq(4,4000,4),1),] > x))/((1-x)*1400))

#FDR Goal without any change to anything
(0.75 * 1400 * 0.10) / (mean(sapply(seq(4,4000,4),FUN = function(x) sum(dgp2_25[x,]<0.10))))
1400*((0.75 * 1400 * 0.10) / (mean(sapply(seq(4,4000,4),FUN = function(x) sum(dgp2_25[x,]<0.10)))))
dgp2_25[i,] 

####
######## Harvey Presidential ######## 
# Si tests son bajo una normal el MBF = e((-Z**2)/2)
# Ej. un Z = 2, t-value, es una prob = 0.977 que es un p-value=5% 2-tails
# Bayenized p-value: 
exp((-2**2)/2)

#We normalize the values that we will evaluate
meanpost <- mean(xpost[,1])
sdpost <- sd(xpost[,1])
######## 