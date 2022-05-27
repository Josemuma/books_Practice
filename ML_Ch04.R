library(pacman)
p_load(mlr, tidyverse, titanic, mlr3)

# Logistic Regression ####
# Data to model to the class with highest probability 
# Logistic regression learns models that estimate the 
# probability of new cases belonging to each class.

# Survive Titanic ####
data(titanic_train)
titanicTib <- as_tibble(titanic_train)

### Clean Data ####
# Convert to factor b/c represent differences between cases
# Feature engineering (modify var to improve prediction)
fctrs <- c('Survived','Sex','Pclass')
titanicClean <- titanicTib  %>% mutate_at(.vars = fctrs, .funs = factor) %>% 
  mutate(FamSize = SibSp + Parch) %>% select(Survived, Pclass, Sex, Age, Fare, FamSize)
titanicClean

### Plot ####
titanicUntidy <- gather(titanicClean, key = 'Variable', value = 'Value', -Survived)
titanicUntidy

titanicUntidy %>% 
  filter(Variable != 'Pclass' & Variable != 'Sex') %>% 
  ggplot(aes(Survived, as.numeric((Value)))) +
  facet_wrap(~ Variable, scales = 'free_y') +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  theme_bw()

titanicUntidy %>% 
  filter(Variable == "Pclass" | Variable == "Sex") %>% 
  ggplot(aes(Value, fill = Survived)) + 
  facet_wrap(~ Variable, scales = "free_x") + 
  geom_bar(position = "fill") + theme_bw()

## Train Model ####
# Create task, learner and model
# Set how we want the prediction to be (a probability) instead
# of the class it would belong to. 
titanicTask <- makeClassifTask(data = titanicClean, target = 'Survived')
logreg <- makeLearner('classif.logreg', predict.type = 'prob')
logRegModel <- train(logreg, titanicTask) # Task and Learner
### Missing Data ####
# Corregir el tema de un dato faltante
titanicClean$Age
sum(is.na(titanicClean$Age))
### Impute Missing ####
imp <- impute((titanicClean), cols = list(Age = imputeMean()))
sum(is.na(titanicClean$Age))
sum(is.na(imp$data$Age))























