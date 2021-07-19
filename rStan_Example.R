
I am new at using Stan in R and want to know if what I am doing is possible or I am probably
replicating unnecessary code. My goal is to understand if there is a way to create a model that is a  substitute 
than running 1,000 times a stan_glm regression. 
My data is as follows:
I have 1,000 different funds. Each fund has its own vector of returns (dependent variable), 240 values.
Each regression is ran against the same matrix of independent variables. I have a total of 3 independent 
variables. 

Using stan_glm it looks like this:
  returns  <- replicate(1000, rnorm(240,0,1)) #That way a have the 1,000 vectors with their `returns`
  independenteVar <-  replicate(3, rnorm(240,0,1)) 
  stan_glm(returns[,1] ~ ., data = independentVar, refresh = 0)
  
So far here, I am running this 1,000 times (I do this as a Local Job, since it takes quite some time).
The idea of this is that I would have results for each fund based on their returns. 
Now, is there a way to do this faster/more efficient with a proper Stan model.

If more RC is necessary, I will try my best. As of now I do not care much about the priors, I am more interested in knowing how to do this. 
Also, any reference to read, I will apreciate it. I felt that the Stan exampled do not solve my question, although,
I am probably wrong. 
Thank you.


