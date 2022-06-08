##### Libraries #####
library(fitdistrplus)
library(RColorBrewer)
library(ggplot2)
library(bayesboot)
library(boot)
library(DirichletReg)
library(extraDistr)
library(parameters)
library(see)
library(bayestestR)
library(BayesFactor)
library(mixtools)

#####

##### Data to be read and  input #####
xx15 <- readRDS("xData15.rds")
xx25 <- readRDS("xData25.rds")
xx35 <- readRDS("xData35.rds")
xpost15 <- readRDS("xPost15.rds")
xpost25 <- readRDS("xPost25.rds")
xpost35 <- readRDS("xPost35.rds")
dgp2_15 <- readRDS("DGP02_15.rds")
dgp2_25 <- readRDS("DGP02_25.rds")
dgp2_35 <- readRDS("DGP02_35.rds")

alphas <- matrix(c(-1.5,-2.5,-3.5,-4.5,0,0,0,0,1.5,2.5,3.5,4.5), 4, 3)
#####



#Convert to p-values
rm(borrar)
borrar <- as.data.frame(xpost25[1,])
borrar
#We take the SE for each of the 1,400 cases
se_perFund <- xx25$SESAlpha
#We obtain an approximate p-value from the alphas in the Bayesian regression
borrar["CoefDivSe"] <-  as.data.frame(borrar/se_perFund)
borrar["pBay"] <- 2*pt(borrar$CoefDivSe,379, FALSE)



#Beta odd version with p-values
pValuedf <- as.data.frame(0) 
minValue = 0.05
lamb = 0.5
for (i in 1:100) {
  sampletemp <- as.data.frame(dgp2_25[sample(seq(4,4000,4),1),])
  pValuedf[i,1] = sum(sampletemp > minValue) 
  pValuedf[i,2] <- (sum(sampletemp > minValue))/1400  #ratio

  #sampletemp2 <- as.data.frame(sample(pd_to_p(dgp2_25[4*i,]),1400,replace = TRUE)) #Traditional Bootstrap
  #pValuedf[i,3] = sum(sampletemp2 > minValue) 
  #pValuedf[i,4] <- (sum(sampletemp2 > minValue))/1400  #ratio
} 
rm(sampletemp, sampletemp2)
colMeans(pValuedf)
colnames(pValuedf) <- c('NoOfNonZeroTB', 'RatioTB')
head(pValuedf)
#Vamos a  ajustar una Beta a los ratios 
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
hist(rbeta(length(pValuedf$RatioBB),fitbetaIdeal[1],fitbetaIdeal[2]), col = 'red') #Los ideal
hist(pValuedf$RatioTB)
hist(rbeta(length(pValuedf$RatioBB),fitBeta2$estimate[1],fitBeta2$estimate[2]), col = 'blue', add = TRUE) #they are the same 

#BB
hist(pValuedf$RatioBB)
hist(rbeta(length(pValuedf$RatioBB),fitBeta$estimate[1],fitBeta$estimate[2]), col = 'blue', add = TRUE) #they are the same
hist(rbeta(length(pValuedf$RatioBB),fitbetaIdeal[1],fitbetaIdeal[2]), col = 'red', add = TRUE) #Los ideal

#PEP Columns
pValuedf$UpdBB <- (pValuedf$NoOfNonZeroBB + fitBeta$estimate[1]) /(1400 + fitBeta$estimate[2])
pValuedf$UpdBBId <- (pValuedf$NoOfNonZeroBB + fitbetaIdeal[1]) /(1400 + fitbetaIdeal[2])
pValuedf$UpdTB <- (pValuedf$NoOfNonZeroTB + fitBeta2$estimate[1]) /(1400 + fitBeta2$estimate[2])
pValuedf$UpdTBId <- (pValuedf$NoOfNonZeroTB + fitbetaIdeal[1]) /(1400 + fitbetaIdeal[2])

i=sample(1:1400,1)
for (j in seq(.75,.82,0.01)) {
print(pbeta(j, pValuedf$NoOfNonZeroBB[i]+ fitBeta$estimate[1], (1400-pValuedf$NoOfNonZeroBB[i]+fitBeta$estimate[2])))
print(pbeta(j, pValuedf$NoOfNonZeroBB[i]+ fitbetaIdeal[1], (1400-pValuedf$NoOfNonZeroBB[i]+fitbetaIdeal[2])))
print(pbeta(j, pValuedf$NoOfNonZeroTB[i]+ fitBeta2$estimate[1], (1400-pValuedf$NoOfNonZeroTB[i]+fitBeta2$estimate[2])))
print(pbeta(j, pValuedf$NoOfNonZeroTB[i]+ fitbetaIdeal[1], (1400-pValuedf$NoOfNonZeroTB[i]+fitbetaIdeal[2])))
}

#PEP looks like this
pValuedf$PEPBB <- pbeta(0.78, pValuedf$NoOfNonZeroBB + fitBeta$estimate[1], (1400-pValuedf$NoOfNonZeroBB+fitBeta$estimate[2]))
pValuedf$PEPBBId <- pbeta(0.78,pValuedf$NoOfNonZeroBB+ fitbetaIdeal[1], (1400-pValuedf$NoOfNonZeroBB+fitbetaIdeal[2]))
pValuedf$PEPTB <- pbeta(0.78, pValuedf$NoOfNonZeroTB+ fitBeta2$estimate[1], (1400-pValuedf$NoOfNonZeroTB+fitBeta2$estimate[2]))
pValuedf$PEPTBId <- pbeta(0.78, pValuedf$NoOfNonZeroTB+ fitbetaIdeal[1], (1400-pValuedf$NoOfNonZeroTB+fitbetaIdeal[2]))

hist(pValuedf$PEPBB)
hist(pValuedf$PEPBBId)
hist(pValuedf$PEPTB)
hist(pValuedf$PEPTBId)

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
qplot(pValuedf$RatioTB,pValuedf$MeanPostTBId, colour = rbPal) + scale_colour_gradient(low = "green", high = "black") +
  geom_hline(yintercept= priorFittedTB, linetype="dashed", color = "red") +
  geom_abline(slope=1, intercept=0, color = "red")

#FDR Finally?
#Buscamos calcular el area a la izquierda de la linea roja
# Prob de que sea un  FD pbeta(0.25, 60,140)

#Lo revisamos para cada una de las estrategias
pValuedf$FDRBB <- pbeta(0.77,pValuedf$NoOfNonZeroPostBB, pValuedf$NoOfNonZeroPostBetaBB)
pValuedf$FDRBBId <- pbeta(0.77,pValuedf$NoOfNonZeroPostBBId, pValuedf$NoOfNonZeroPostBetaBBId)
pValuedf$FDRTB <- pbeta(0.77,pValuedf$NoOfNonZeroPostTB, pValuedf$NoOfNonZeroPostBetaTB)
pValuedf$FDRTBId <- pbeta(0.77,pValuedf$NoOfNonZeroPostTBId, pValuedf$NoOfNonZeroPostBetaTBId)

(sum(pValuedf$FDRBB > 0.10))/1000
(sum(pValuedf$FDRBBId > 0.10))/1000
(sum(pValuedf$FDRTB > 0.10))/1000
(sum(pValuedf$FDRTBId > 0.10))/1000

sum(pValuedf$FDRBB < 0.10)/1000
sum(pValuedf$FDRTB < 0.10)/1000
sum(pValuedf$FDRBBId < 0.10)/1000
sum(pValuedf$FDRTBId < 0.10)/1000

sapply(seq(0.05,.9,0.05), FUN = function(x) (sum(pValuedf$FDRBB < x))/((1-x)*1000))
sapply(seq(0.05,.9,0.05), FUN = function(x) (sum(pValuedf$FDRTB < x))/((1-x)*1000))
sapply(seq(0.05,.9,0.05), FUN = function(x) (sum(pValuedf$FDRBBId < x))/((1-x)*1000))
sapply(seq(0.05,.9,0.05), FUN = function(x) (sum(pValuedf$FDRTBId < x))/((1-x)*1000))
sapply(seq(0.5,.8,0.05), FUN = function(x) (sum(dgp2_25[sample(seq(4,4000,4),1),] > x))/((1-x)*1400))
mean(sapply(seq(4,4000,4), FUN =  function(x) (sum(dgp2_35[sample(seq(4,4000,4),1),] > .6) / ((1-.6)*1400))))

#FDR Goal without any change to anything
(0.75 * 1400 * 0.05) / (mean(sapply(seq(4,4000,4),FUN = function(x) sum(dgp2_25[x,]<0.05))))
dgp2_25[i,] 

####
# We take the posterior probabilities from the bayesian models. We draw 1,000 alphas per fund and calculate its median. From a Bayesian
#  perspective the posterior median is the value that minimizes (CITA) Bayesian  risk. We evaluate if the median of each draw of  posteriors falls
# outside of the zero zone. We define the zero zone as that within +/- the monthly alpha. We remind the reader that our main interest and contribution
# is to have a better estimation of PI0. 
# 

rangeZero <- c(alphN[2],alphP[2])

temp <- abs(xpost[,1])
tempFit <- fitdist(temp, "beta")
tempFit
hist(rbeta(1000, tempFit$estimate[1], tempFit$estimate[2]))
temp <- abs(xpost[,1300])
tempFit <- fitdist(temp, "beta")
tempFit
hist(rbeta(1000, tempFit$estimate[1], tempFit$estimate[2]))
temp <- abs(xpost[,1399])
tempFit <- fitdist(temp, "beta")
tempFit
hist(rbeta(1000, tempFit$estimate[1], tempFit$estimate[2]))
hist(rbeta(1000,1,1400), col = 'red', add = TRUE) #Los ideal
rm( tempFit ,temp, sumTemp, temp2, temp3)

###### Average of Pi0 for all the cases of the regressions and each of the alphas proposed. ######
lambda = 0.50
mean(sapply(1:1000, function(x) (sum(dgp2_15[4*x,] > lambda)) / ((1-lambda)*1400)))
mean(sapply(1:1000, function(x) (sum(dgp2_25[4*x,] > lambda)) / ((1-lambda)*1400)))
mean(sapply(1:1000, function(x) (sum(dgp2_35[4*x,] > lambda)) / ((1-lambda)*1400)))
mean(sapply(1:1000, function(x) (sum(dgp2_45[4*x,] > lambda)) / ((1-lambda)*1400)))
######

###### Optimal method ######
#El minimo para cada uno de los 1,000 escenarios
min_lam <- as.data.frame(0)
for(i in 1:1000){ #todos los escenarios
  #print(sapply(1:19, function(x) (sum(dgp2_25[4*i,] > storey_lambda[x])) / ((1-storey_lambda[x])*1400)))  #19 por esccenario
  #print(min(sapply(1:19, function(x) (sum(dgp2_25[4*i,] > storey_lambda[x])) / ((1-storey_lambda[x])*1400))))  #el minimo pi
  min_lam[i,] <- (storey_lambda[which.min(sapply(1:19, function(x) (sum(dgp2_25[4*i,] > storey_lambda[x])) / ((1-storey_lambda[x])*1400)))]) #el lam correspondiente
}
min_lam

#Con todos los valores de lambda (solo 5 corridas)
i = 1 
replicate(5,sapply(1:19, function(x) ((sum(sample(dgp2_25[4*i,],1400,TRUE)>storey_lambda[x])) / 
                                         ((1-storey_lambda[x])*1400)-
                                         (sum(dgp2_25[4*i,]>min_lam[i,]))/((1-min_lam[i,])*1400))**2))
#La media por renglon(por lambda) son 1,000 corridas
rowmeans01 <- rowMeans(replicate(1000,sapply(1:19, function(x) ((sum(sample(dgp2_25[4*i,],1400,TRUE)>storey_lambda[x])) / 
                                                                      ((1-storey_lambda[x])*1400)-
                                                                      (sum(dgp2_25[4*i,]>min_lam[i,]))/((1-min_lam[i,])*1400))**2)))
#El lambda que minimiza:
storey_lambda[which.min(rowmeans01)]
#El Pi0 que obtenemos:
sum(dgp2_25[4,]>storey_lambda[which.min(rowmeans01)])/((1-storey_lambda[which.min(rowmeans01)])*1400)
sapply(1:length(storey_lambda), function(x) (sum(dgp2_25[4,] > storey_lambda[x])) / ((1-storey_lambda[x])*1400))
######

