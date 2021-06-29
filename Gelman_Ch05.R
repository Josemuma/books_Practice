#Prob de cuantos bebes que nacieron son niñas (400 nacimientos)
#Simulamos 400 nacimientos
n_girls <-  rbinom(1, 400, 0.488) #la prob de que sea niña es conocida
print(n_girls)

#Para tener una idea de la distribucion que vamos a tener lo revisamos 1,000 veces
n_sims <- 1000 
n_girls <- rep(NA, n_sims)
for(s in 1:n_sims){
  n_girls[s] <- rbinom(1, 400, 0.488) 
}
hist(n_girls)
#Las 1,000 simulaciones capturan la incertidumbre
#Opc B sin el loop
n_girls <-  rbinom(n_sims, 400, 0.488)
hist(n_girls)
#Lo podemos hacer con REPLICATE tambien


##################
#Chapter 06
library("rprojroot")
#root<-has_file(".ROS-Examples-root")$make_fix_file()
library("rstanarm")
library("HistData")

#heights <- as.data.frame(as.numeric(read.table("Heights.txt", header=TRUE)))
heights <- read.csv("Heights.csv", header = TRUE)
n <- nrow(heights)
head(heights)
#1
fit_1 <- stan_glm(heights$daughter_height ~ heights$mother_height, data = heights)
print(fit_1)
#2 Graph Data
mother_height_jitt <- heights$mother_height + runif(n, -0.5, 0.5)
daughter_height_jitt <- heights$daughter_height + runif(n, -0.5, 0.5)
plot(mother_height_jitt, daughter_height_jitt, xlab = "Mother's height(in", ylab = "Adult Daughter's height(in)")
#Coefficients from the regression
a_hat <- coef(fit_1)[1]
b_hat <- coef(fit_1)[2]
abline(a_hat, b_hat)

#MID-TERM Example with fake data
n <- 1000
true_ability <- rnorm(n,50,10)
noise_1 <- rnorm(n,0,10)
noise_2 <- rnorm(n,0,10)
midterm <- true_ability + noise_1
final <- true_ability + noise_2
exams <-  data.frame(midterm,final)
fit_1 <- stan_glm(final ~ midterm, data = exams)
plot(midterm, final, xlab = "Midterm exam score", ylab = "Final exam score")
abline(coef(fit_1))

