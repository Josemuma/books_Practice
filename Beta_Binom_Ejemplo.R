library(pacman)
p_load(bayesrules, tidyverse)

# Tuning Beta prior
# I have to always tune alpha and beta of the B Distribution. In this example 
# we are looking at an election scenario. 
# Lowest poll was 25% and highest 65%
# Average poll 45% across 30 polls
# We need to look for alpha and beta where E(pi) = 45 = a/(a+b)
## Aqui una opcion de estimar parametros !!!

plot(seq(0, 1, length=100),dbeta(seq(0, 1, length=100), 45, 55), type='l')
lines(seq(0, 1, length=100),dbeta(seq(0, 1, length=100), 27, 33), type='l',col='red')
plot_beta(45,55)

#Then, a reasonable prior of candidate: pi~Beta(45,55)
#f(pi) =(T(45+55)/T(45)T(55)) pi^45-1(1-pi)^100-55-1

# Y|pi ~ Binomial (n, pi), if n = 50
# f(y|pi) = (50 en y)pi^y(1-pi)^50-y

### The question we are answering is:
# if the support of the candidate was a given value of pi, then how many of the n voters
# might we expect to support

# Let me assume the result is 30 out of the 50 
# Puedo comparar en distintos niveles de pi {0.1,...0.9}, c'omo se ve la posibilidad
# de haver obtenido 30/50 para el candidato

## Likelihood es entonces L(pi|y=30), es como si vieramos Y=30 observed data y pi como unknown
# L is a function of pi and tells us the compatibility of the observed polling data Y=30 with
# differnt pi values.

# Posterior Beta
plot_beta_binomial(45,55,30,50)
# pi|Y=30 ~ Beta(75,75)
summarize_beta_binomial(45,55,30,50)
# We appreciate how her E(pi) moved from .45 to .50

#Simulation
set.seed(84735)
m_sim <- data.frame(pi=rbeta(10000,45,55)) %>% 
  mutate(y =rbinom(10000,size=50,prob = pi))
m_sim
ggplot(m_sim, aes(x=pi, y=y)) +
  geom_point(aes(color=(y==30)), size = 0.1)
m_posterior <- m_sim %>% filter(y == 30)

ggplot(m_posterior, aes(x=pi)) +
  geom_density()
m_posterior %>% summarize(mean(pi), sd(pi))
nrow(m_posterior) #No. of sim that matched the y = 30 out of 10,000

## Example 2
plot_beta(1,10) #A small prop of the people do what they are told
# n = 40 los que mas shock van a dar
summarize_beta_binomial(1,10,26,40)
plot_beta_binomial(1,10,26,40 )

rm(m_sim, m_posterior)
