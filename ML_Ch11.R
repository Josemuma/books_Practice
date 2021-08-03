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

Ex. when we are calculate the residuales of a linear regression, we get the
Sum of Squares (Sum(y-yestim)^2)). In ML it is the Loss function of the OLS algorithm

L2 norm is used in ridge regression to modify slightly the SoS. Include a term 
that makes the function's value larger, the larger the parameter estimates are. 
Un termino que hace a la funcion mas grande, mientras mas grandes sean los parametros
estimados. 
1. Calculate SoS
2. Calculate L2 norm (si solo hay un predictor, es el cuadrado de la pendiente, si
tenemos 2 la suma del cuadrado de las pendientes)
3. SoS + Lambda * L2 norm
Mayor lambda, mayor la penalizacion al modelo. 
Lambda es un hiperparametro, necesario tunear y se puede estimar con CV

*** Always scale the predictor variables previous to L2 or L1 penalty loss functions


































